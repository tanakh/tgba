#[macro_use]
extern crate prettytable;

// mod gdb;

use anyhow::{anyhow, bail, Result};
use compress_tools::{list_archive_files, uncompress_archive_file};
use log::info;
use sdl2::{
    audio::{AudioQueue, AudioSpecDesired},
    controller::{Button, GameController},
    event::Event,
    keyboard::{Keycode, Scancode},
    pixels::Color,
    surface::Surface,
    EventPump,
};
use std::{
    fs::{read, File},
    io::Read,
    path::Path,
    time::Duration,
};
use tempfile::NamedTempFile;

use tgba_core::{Agb, KeyInput, Rom};

const SCREEN_WIDTH: u32 = 240;
const SCREEN_HEIGHT: u32 = 160;
const SCALING: u32 = 4;

pub fn run(bios: &Path, rom: &Path) -> Result<()> {
    env_logger::builder().format_timestamp(None).init();

    let bios = read(&bios)?;
    let rom = load_rom(&rom)?;

    dump_rom_info(&rom);

    let mut agb = Agb::new(bios, rom);

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

    let game_controller = game_controller_subsystem.open(0).ok();

    while process_events(&mut event_pump) {
        let start_time = std::time::Instant::now();

        let key_input = get_key_input(&event_pump, &game_controller);
        // eprintln!("Key input: {key_input:?}");

        // if event_pump
        //     .keyboard_state()
        //     .is_scancode_pressed(Scancode::Space)
        // {
        agb.set_key_input(&key_input);
        agb.run_frame();
        // }

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

fn get_key_input(e: &EventPump, game_controller: &Option<GameController>) -> KeyInput {
    let ks = e.keyboard_state();

    let mut ret = KeyInput::default();
    ret.a = ks.is_scancode_pressed(Scancode::X);
    ret.b = ks.is_scancode_pressed(Scancode::Z);
    ret.select = ks.is_scancode_pressed(Scancode::RShift);
    ret.start = ks.is_scancode_pressed(Scancode::Return);
    ret.right = ks.is_scancode_pressed(Scancode::Right);
    ret.left = ks.is_scancode_pressed(Scancode::Left);
    ret.up = ks.is_scancode_pressed(Scancode::Up);
    ret.down = ks.is_scancode_pressed(Scancode::Down);
    ret.r = ks.is_scancode_pressed(Scancode::S);
    ret.l = ks.is_scancode_pressed(Scancode::A);

    if let Some(game_controller) = game_controller {
        ret.a = game_controller.button(Button::B);
        ret.b = game_controller.button(Button::A);
        ret.select = game_controller.button(Button::Back);
        ret.start = game_controller.button(Button::Start);
        ret.right = game_controller.button(Button::DPadRight);
        ret.left = game_controller.button(Button::DPadLeft);
        ret.up = game_controller.button(Button::DPadUp);
        ret.down = game_controller.button(Button::DPadDown);
        ret.r = game_controller.button(Button::RightShoulder);
        ret.l = game_controller.button(Button::LeftShoulder);
    }

    ret
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
