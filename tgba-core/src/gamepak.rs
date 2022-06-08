use crate::{backup::Backup, rom::Rom, util::read16};
use log::warn;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct GamePak {
    #[serde(skip)]
    rom: Rom,
    backup: Backup,
}

impl GamePak {
    pub fn new(rom: Rom, backup: Option<Vec<u8>>) -> Self {
        let backup = Backup::detect_backup(&rom.data, backup);
        Self { rom, backup }
    }

    pub fn rom(&self) -> &Rom {
        &self.rom
    }

    pub fn rom_mut(&mut self) -> &mut Rom {
        &mut self.rom
    }

    pub fn backup(&self) -> &Backup {
        &self.backup
    }

    pub fn backup_mut(&mut self) -> &mut Backup {
        &mut self.backup
    }

    pub fn is_valid_eeprom_addr(&self, addr: u32) -> bool {
        let large_rom = self.rom.data.len() > 0x01000000;
        (!large_rom && addr & 0x01000000 != 0) || (large_rom && addr & 0x01FFFF00 == 0x01FFFF00)
    }

    pub fn read(&mut self, addr: u32) -> u16 {
        if self.is_valid_eeprom_addr(addr) {
            return self.backup.read_eeprom() as u16;
        }

        if (addr as usize & 0x01FFFFFE) >= self.rom.data.len() {
            warn!("Read from invalid Game Pak ROM address: 0x{addr:08X}");
            return 0;
        }

        read16(&self.rom.data, addr as usize)
    }

    pub fn write(&mut self, addr: u32, data: u16) {
        if self.is_valid_eeprom_addr(addr) {
            self.backup.write_eeprom(data & 1 != 0);
        } else {
            warn!("Write to invalid Game Pak ROM address: 0x{addr:08X} = 0x{data:04X}");
        }
    }
}
