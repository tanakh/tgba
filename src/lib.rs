#[macro_use]
extern crate prettytable;

// mod gdb;

use anyhow::{anyhow, bail, Result};
use compress_tools::{list_archive_files, uncompress_archive_file};
use log::info;
use sdl2::{event::Event, keyboard::Keycode, pixels::Color, surface::Surface};
use std::{
    fs::{read, File},
    io::Read,
    path::Path,
    time::Duration,
};
use tempfile::NamedTempFile;

use tgba_core::{Agb, Rom};

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

    let mut canvas = window.into_canvas().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 255, 255));
    canvas.clear();
    canvas.present();
    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut i = 0;

    let texture_creator = canvas.texture_creator();
    let mut surface = Surface::new(
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        sdl2::pixels::PixelFormatEnum::RGB24,
    )
    .unwrap();

    'running: loop {
        i = (i + 1) % 255;
        canvas.set_draw_color(Color::RGB(0, 0, 0));
        canvas.clear();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

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
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }

    Ok(())
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
