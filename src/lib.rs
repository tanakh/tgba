#[macro_use]
extern crate prettytable;

// mod gdb;

use anyhow::{anyhow, bail, Result};
use compress_tools::{list_archive_files, uncompress_archive_file};
use log::{error, info};
use sdl2::{
    audio::{AudioQueue, AudioSpecDesired},
    controller::{Button, GameController},
    event::Event,
    keyboard::{KeyboardState, Keycode, Scancode},
    pixels::Color,
    surface::Surface,
    EventPump,
};
use std::{
    fs::{read, File},
    io::Read,
    path::{Path, PathBuf},
    time::Duration,
};
use tempfile::NamedTempFile;

use tgba_core::{Agb, KeyInput, Rom};

const SCREEN_WIDTH: u32 = 240;
const SCREEN_HEIGHT: u32 = 160;
const SCALING: u32 = 4;

pub fn run(bios: &Path, rom_path: &Path) -> Result<()> {
    env_logger::builder().format_timestamp(None).init();

    let bios = read(&bios)?;
    let rom = load_rom(&rom_path)?;
    let backup = load_backup(&rom_path)?;

    dump_rom_info(&rom);

    let mut agb = Agb::new(bios, rom, backup);

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("TGBA", SCREEN_WIDTH * SCALING, SCREEN_HEIGHT * SCALING)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    let mut event_pump = sdl_context.event_pump().unwrap();

    let texture_creator = canvas.texture_creator();
    let mut surface = Surface::new(
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        sdl2::pixels::PixelFormatEnum::RGB24,
    )
    .unwrap();

    const SOUND_BUF_LEN: u16 = 2048;

    let audio_subsystem = sdl_context.audio().map_err(|e| anyhow!("{e}"))?;
    let desired_spec = AudioSpecDesired {
        freq: Some(48000),
        channels: Some(2),
        samples: Some(SOUND_BUF_LEN),
    };
    let audio_queue: AudioQueue<i16> = audio_subsystem
        .open_queue(None, &desired_spec)
        .map_err(|e| anyhow!("{e}"))?;
    audio_queue.queue_audio(&vec![0; 2048]).unwrap();
    audio_queue.resume();

    let game_controller_subsystem = sdl_context.game_controller().unwrap();

    let game_controller = game_controller_subsystem.open(0).ok().into_iter().collect();

    let mut im = InputManager::new(game_controller);

    let mut frames = 0;
    let mut cur_slot = 0_u32;

    while process_events(&mut event_pump) {
        let start_time = std::time::Instant::now();

        im.update(&event_pump);

        if im.hotkey_pressed(HotKey::SaveState) {
            save_state(&agb, &rom_path, cur_slot)?;
        }
        if im.hotkey_pressed(HotKey::LoadState) {
            if let Err(err) = load_state(&mut agb, &rom_path, cur_slot) {
                error!("Failed to load state from slot {cur_slot}: {err}");
            }
        }

        if im.hotkey_pressed(HotKey::NextSlot) {
            cur_slot += 1;
            info!("State save slot changed: {cur_slot}");
        }
        if im.hotkey_pressed(HotKey::PrevSlot) {
            cur_slot = cur_slot.saturating_sub(1);
            info!("State save slot changed: {cur_slot}");
        }

        let key_input = im.key_input();
        agb.set_key_input(&key_input);
        agb.exec_frame();

        let frame_buf = agb.frame_buf();

        surface.with_lock_mut(|buf| {
            for y in 0..SCREEN_HEIGHT {
                for x in 0..SCREEN_WIDTH {
                    let p = frame_buf.pixel(x, y);
                    let ix = (y * SCREEN_WIDTH + x) as usize * 3;
                    buf[ix] = p.r;
                    buf[ix + 1] = p.g;
                    buf[ix + 2] = p.b;
                }
            }
        });

        let texture = surface.as_texture(&texture_creator).unwrap();
        canvas.copy(&texture, None, None).unwrap();

        canvas.present();

        let audio_buf = agb.audio_buf();
        // assert!(
        //     (799..=801).contains(&audio_buf.len()),
        //     "invalid generated audio length: {}",
        //     audio_buf.len()
        // );

        // Sync by audio
        while audio_queue.size() > SOUND_BUF_LEN as u32 * 4 {
            std::thread::sleep(Duration::from_millis(1));
        }

        audio_queue
            .queue_audio(
                &audio_buf
                    .buf
                    .iter()
                    .flat_map(|s| [s.left, s.right])
                    .collect::<Vec<i16>>(),
            )
            .unwrap();

        // let elapsed = std::time::Instant::now() - start_time;
        // let wait = std::time::Duration::from_nanos(1_000_000_000u64 / 60);
        // if wait > elapsed {
        //     std::thread::sleep(wait - elapsed);
        // }

        frames += 1;

        if frames % (60 * 60) == 0 {
            if let Some(backup) = agb.backup() {
                save_backup(&rom_path, backup)?;
            }
        }
    }

    if let Some(backup) = agb.backup() {
        save_backup(&rom_path, backup)?;
    }

    Ok(())
}

fn process_events(event_pump: &mut EventPump) -> bool {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => return false,
            _ => {}
        }
    }
    true
}

struct InputManager {
    controllers: Vec<GameController>,
    key_bind: Vec<(Key, KeyBind)>,
    cur_key_input: Vec<bool>,
    hotkey: Vec<(HotKey, KeyBind)>,
    cur_hotkey: Vec<bool>,
    prev_hotkey: Vec<bool>,
}

enum Key {
    A,
    B,
    Select,
    Start,
    Right,
    Left,
    Up,
    Down,
    R,
    L,
}

#[derive(PartialEq, Eq)]
enum HotKey {
    SaveState,
    LoadState,
    NextSlot,
    PrevSlot,
}

enum KeyBind {
    Scancode(Scancode),
    Button(Button),
    And(Vec<KeyBind>),
    Or(Vec<KeyBind>),
}

macro_rules! kbd {
    ($key:ident) => {
        KeyBind::Scancode(Scancode::$key)
    };
}

macro_rules! pad {
    ($button:ident) => {
        KeyBind::Button(Button::$button)
    };
}

impl std::ops::BitOr for KeyBind {
    type Output = KeyBind;

    fn bitor(self, rhs: Self) -> Self::Output {
        KeyBind::Or(vec![self, rhs])
    }
}

macro_rules! def_key_bind {
    ($key:path => $e:expr, $($rest:tt)*) => {
        def_key_bind!($($rest)* ($key, $e))
    };
    ($(($key:path, $e:expr))*) => {
        vec![ $( ($key, $e) ,)* ]
    };
}

fn default_key_bind() -> Vec<(Key, KeyBind)> {
    use Key::*;
    def_key_bind! {
        A => kbd!(X) | pad!(B),
        B => kbd!(Z) | pad!(A),
        Select => kbd!(RShift) | pad!(Back),
        Start => kbd!(Return) | pad!(Start),
        Right => kbd!(Right) | pad!(DPadRight),
        Left => kbd!(Left) | pad!(DPadLeft),
        Up => kbd!(Up) | pad!(DPadUp),
        Down => kbd!(Down) | pad!(DPadDown),
        R => kbd!(S) | pad!(RightShoulder),
        L => kbd!(A) | pad!(LeftShoulder),
    }
}

fn default_hotkey() -> Vec<(HotKey, KeyBind)> {
    use HotKey::*;
    def_key_bind! {
        SaveState => kbd!(F5),
        LoadState => kbd!(F7),
        NextSlot => kbd!(F3),
        PrevSlot => kbd!(F2),
    }
}

impl InputManager {
    fn new(controllers: Vec<GameController>) -> Self {
        let key_bind = default_key_bind();
        let hotkey = default_hotkey();

        Self {
            controllers,
            cur_key_input: vec![false; key_bind.len()],
            key_bind,
            cur_hotkey: vec![false; hotkey.len()],
            prev_hotkey: vec![false; hotkey.len()],
            hotkey,
        }
    }

    fn update(&mut self, e: &EventPump) {
        let ks = e.keyboard_state();

        for (i, (_, key_bind)) in self.key_bind.iter().enumerate() {
            self.cur_key_input[i] = self.pressed(&ks, key_bind);
        }

        self.prev_hotkey.copy_from_slice(&self.cur_hotkey);

        for (i, (_, key_bind)) in self.hotkey.iter().enumerate() {
            self.cur_hotkey[i] = self.pressed(&ks, key_bind);
        }
    }

    fn pressed(&self, ks: &KeyboardState, key_bind: &KeyBind) -> bool {
        match key_bind {
            KeyBind::Scancode(sc) => ks.is_scancode_pressed(*sc),
            KeyBind::Button(button) => match self.controllers.get(0) {
                Some(c) => c.button(*button),
                _ => false,
            },
            KeyBind::And(keys) => keys.iter().all(|key| self.pressed(ks, key)),
            KeyBind::Or(keys) => keys.iter().any(|key| self.pressed(ks, key)),
        }
    }

    fn key_input(&self) -> KeyInput {
        let mut ret = KeyInput::default();

        for (i, (key, _)) in self.key_bind.iter().enumerate() {
            *match key {
                Key::A => &mut ret.a,
                Key::B => &mut ret.b,
                Key::Select => &mut ret.select,
                Key::Start => &mut ret.start,
                Key::Right => &mut ret.right,
                Key::Left => &mut ret.left,
                Key::Up => &mut ret.up,
                Key::Down => &mut ret.down,
                Key::R => &mut ret.r,
                Key::L => &mut ret.l,
            } = self.cur_key_input[i];
        }

        ret
    }

    fn hotkey_pressed(&self, hotkey: HotKey) -> bool {
        for (i, (key, _)) in self.hotkey.iter().enumerate() {
            if *key == hotkey {
                return !self.prev_hotkey[i] && self.cur_hotkey[i];
            }
        }
        false
    }
}

fn load_rom(file: &Path) -> Result<Rom> {
    match file.extension().and_then(|ext| ext.to_str()) {
        Some("zip" | "7z") => {
            let gba_file = {
                let mut source = File::open(file)?;

                list_archive_files(&mut source)?
                    .into_iter()
                    .find(|f| Path::new(f).extension().and_then(|ext| ext.to_str()) == Some("gba"))
                    .ok_or_else(|| anyhow!("No GBA ROM found in archive"))?
            };

            info!("Loading ROM file: `{gba_file}`");

            let mut source = File::open(file)?;
            let mut temp = NamedTempFile::new()?;
            uncompress_archive_file(&mut source, &mut temp, &gba_file)?;

            let mut temp = temp.reopen()?;
            let mut data = vec![];
            temp.read_to_end(&mut data)?;

            Rom::from_bytes(&data)
        }
        Some("gba") => {
            info!("Loading ROM from: `{}`.", file.display());
            let bytes = std::fs::read(file)?;
            Rom::from_bytes(&bytes)
        }
        _ => bail!("Unsupported file extension"),
    }
}

fn backup_dir() -> Result<PathBuf> {
    let data_dir = dirs::data_dir()
        .ok_or_else(|| anyhow!("Could not find data directory"))?
        .join("tgba");

    if data_dir.exists() {
        if !data_dir.is_dir() {
            bail!("Data directory is not a directory");
        }
    } else {
        std::fs::create_dir_all(&data_dir)?;
    }

    Ok(data_dir)
}

fn load_backup(rom_path: &Path) -> Result<Option<Vec<u8>>> {
    let backup_path = backup_dir()?.join(rom_path.with_extension("sav").file_name().unwrap());

    if backup_path.exists() {
        info!("Loading backup from: `{}`", backup_path.display());
        let ret = std::fs::read(backup_path)?;
        Ok(Some(ret))
    } else {
        Ok(None)
    }
}

fn save_backup(rom_path: &Path, backup: Vec<u8>) -> Result<()> {
    let backup_path = backup_dir()?.join(rom_path.with_extension("sav").file_name().unwrap());

    info!("Saving backup to: `{}`", backup_path.display());
    std::fs::write(backup_path, backup)?;
    Ok(())
}

fn load_state(agb: &mut Agb, rom_path: &Path, slot: u32) -> Result<()> {
    let state_path = backup_dir()?.join(
        rom_path
            .with_extension(format!("{slot}.state"))
            .file_name()
            .unwrap(),
    );
    let data = std::fs::read(state_path)?;
    agb.load_state(&data)?;
    info!("State loaded from slot {slot}");
    Ok(())
}

fn save_state(agb: &Agb, rom_path: &Path, slot: u32) -> Result<()> {
    let state_path = backup_dir()?.join(
        rom_path
            .with_extension(format!("{slot}.state"))
            .file_name()
            .unwrap(),
    );
    std::fs::write(state_path, agb.save_state())?;
    info!("State saved to slot {slot}");
    Ok(())
}

fn dump_rom_info(rom: &Rom) {
    let rom_size = rom.data.len();
    let rom_size = if rom_size < 1 * 1024 * 1024 {
        format!("{} KiB", rom_size as f64 / 1024 as f64)
    } else {
        format!("{} MiB", rom_size as f64 / (1024 * 1024) as f64)
    };

    let mut table = prettytable::table! {
        ["Title", String::from_utf8_lossy(&rom.title)],
        ["Game Code", String::from_utf8_lossy(&rom.game_code)],
        ["Maker Code", String::from_utf8_lossy(&rom.maker_code)],
        ["Main Unit Code", rom.main_unit_code],
        ["Device Type", rom.device_type],
        ["ROM Version", rom.rom_version],
        ["Backup Type", rom.backup_type()],
        ["ROM Size", rom_size]
    };
    table.set_format(*prettytable::format::consts::FORMAT_NO_LINESEP_WITH_TITLE);
    table.printstd();
}
