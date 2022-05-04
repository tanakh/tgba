#[macro_use]
extern crate prettytable;

use anyhow::{anyhow, bail, Result};
use compress_tools::{list_archive_files, uncompress_archive_file};
use log::info;
use std::{
    fs::{read, File},
    io::Read,
    path::{Path, PathBuf},
};
use tempfile::NamedTempFile;

use tgba_core::{Agb, Rom};

#[argopt::cmd]
fn main(bios: PathBuf, rom: PathBuf) -> Result<()> {
    env_logger::builder().format_timestamp(None).init();

    let bios = read(&bios)?;
    let rom = load_rom(&rom)?;

    dump_rom_info(&rom);

    let mut agb = Agb::new(bios, rom);
    loop {
        agb.run_frame();
    }

    Ok(())
}

pub fn load_rom(file: &Path) -> Result<Rom> {
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
