#[macro_use]
extern crate prettytable;

// mod gdb;

use anyhow::{anyhow, bail, Result};
use compress_tools::{list_archive_files, uncompress_archive_file};
use log::info;
use sdl2::{
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
};
use tempfile::NamedTempFile;

use tgba_core::{Agb, KeyInput, Rom};

const SCREEN_WIDTH: u32 = 240;
const SCREEN_HEIGHT: u32 = 160;
const SCALING: u32 = 2;

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

    // sdl_context
    //     .event()
    //     .unwrap()
    //     .flush_event(sdl2::event::EventType::KeyDown);

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    let mut event_pump = sdl_context.event_pump().unwrap();

    let texture_creator = canvas.texture_creator();
    let mut surface = Surface::new(
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        sdl2::pixels::PixelFormatEnum::RGB24,
    )
    .unwrap();

    let mut prev_time = std::time::Instant::now();

    while process_events(&mut event_pump) {
        let key_input = get_key_input(&event_pump);
        eprintln!("Key input: {key_input:?}");
        agb.set_key_input(&key_input);
        agb.run_frame();
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

        let now = std::time::Instant::now();

        let elapsed = now - prev_time;
        let wait = std::time::Duration::from_nanos(1_000_000_000u64 / 60);
        if wait > elapsed {
            std::thread::sleep(wait - elapsed);
        }
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

fn get_key_input(e: &EventPump) -> KeyInput {
    let ks = e.keyboard_state();

    for k in ks.pressed_scancodes() {
        eprintln!("* {k:?}");
    }

    KeyInput {
        a: ks.is_scancode_pressed(Scancode::X),
        b: ks.is_scancode_pressed(Scancode::Z),
        select: ks.is_scancode_pressed(Scancode::RShift),
        start: ks.is_scancode_pressed(Scancode::Return),
        right: ks.is_scancode_pressed(Scancode::Right),
        left: ks.is_scancode_pressed(Scancode::Left),
        up: ks.is_scancode_pressed(Scancode::Up),
        down: ks.is_scancode_pressed(Scancode::Down),
        r: ks.is_scancode_pressed(Scancode::S),
        l: ks.is_scancode_pressed(Scancode::A),
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

fn dump_rom_info(rom: &Rom) {
    let mut table = prettytable::table! {
        ["Title", rom.title],
        ["Game Code", String::from_utf8_lossy(&rom.game_code)],
        ["Maker Code", String::from_utf8_lossy(&rom.maker_code)],
        ["Main Unit Code", rom.main_unit_code],
        ["Device Type", rom.device_type],
        ["ROM Version", rom.rom_version]
    };
    table.set_format(*prettytable::format::consts::FORMAT_NO_LINESEP_WITH_TITLE);
    table.printstd();
}
