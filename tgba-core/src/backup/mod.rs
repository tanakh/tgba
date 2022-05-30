pub mod eeprom;
pub mod flash;
pub mod sram;

use eeprom::Eeprom;
use flash::Flash;
use log::{error, warn};

use self::{eeprom::EepromSize, sram::Sram};

pub enum Backup {
    // EEPROM_Vnnn    EEPROM 512 bytes or 8 Kbytes (4Kbit or 64Kbit)
    Eeprom(Eeprom),
    // SRAM_Vnnn      SRAM 32 Kbytes (256Kbit)
    Sram(Sram),
    // FLASH_Vnnn     FLASH 64 Kbytes (512Kbit) (ID used in older files)
    // FLASH512_Vnnn  FLASH 64 Kbytes (512Kbit) (ID used in newer files)
    // FLASH1M_Vnnn   FLASH 128 Kbytes (1Mbit)
    Flash(Flash),
    Unknown,
}

impl Backup {
    pub fn detect_backup(data: &[u8], backup: Option<Vec<u8>>) -> Backup {
        for i in 0..data.len() {
            if match_id_string(&data[i..], b"EEPROM_Vnnn") {
                return Backup::Eeprom(Eeprom::new(backup));
            }
            if match_id_string(&data[i..], b"SRAM_Vnnn")
                || match_id_string(&data[i..], b"SRAM_F_Vnnn")
            {
                return Backup::Sram(Sram::new(backup));
            }
            if match_id_string(&data[i..], b"FLASH_Vnnn")
                || match_id_string(&data[i..], b"FLASH512_Vnnn")
            {
                // 512Kbit (= 64KB) Flash
                return Backup::Flash(Flash::new(64 * 1024, backup));
            }
            if match_id_string(&data[i..], b"FLASH1M_Vnnn") {
                // 1Mbit (= 128KB) Flash
                return Backup::Flash(Flash::new(128 * 1024, backup));
            }
        }
        if data.is_empty() {
            warn!("Unknown backup type");
        } else {
            error!("Unknown backup type but backup data is given");
        }
        Backup::Unknown
    }

    pub fn data(&self) -> Option<Vec<u8>> {
        match self {
            Backup::Eeprom(e) => Some(e.data()),
            Backup::Sram(s) => Some(s.data()),
            Backup::Flash(f) => Some(f.data()),
            Backup::Unknown => None,
        }
    }

    pub fn backup_type(&self) -> &'static str {
        match self {
            Backup::Eeprom(_) => "EEPROM",
            Backup::Sram(_) => "SRAM",
            Backup::Flash(flash) => flash.backup_type(),
            Backup::Unknown => "Unknown",
        }
    }

    pub fn eeprom_size(&self) -> Option<Option<EepromSize>> {
        if let Backup::Eeprom(eeprom) = self {
            Some(eeprom.size())
        } else {
            None
        }
    }

    pub fn set_eeprom_size(&mut self, size: EepromSize) {
        if let Backup::Eeprom(eeprom) = self {
            eeprom.set_size(size);
        } else {
            warn!("Set EEPROM size to non-EEPROM cartridge: {size:?}");
        }
    }

    pub fn read_eeprom(&mut self) -> bool {
        if let Backup::Eeprom(eeprom) = self {
            eeprom.read()
        } else {
            warn!("Read EEPROM on non-EEPROM cartridge");
            false
        }
    }

    pub fn write_eeprom(&mut self, data: bool) {
        if let Backup::Eeprom(eeprom) = self {
            eeprom.write(data);
        } else {
            warn!("Write EEPROM on non-EEPROM cartridge: {}", data as u8);
        }
    }

    pub fn read_ram(&mut self, addr: u32) -> u8 {
        match self {
            Backup::Sram(sram) => sram.read(addr),
            Backup::Flash(flash) => flash.read(addr),
            _ => {
                warn!("Read GamePak RAM on non-SRAM cartridge: 0x{addr:08X}");
                0
            }
        }
    }

    pub fn write_ram(&mut self, addr: u32, data: u8) {
        match self {
            Backup::Sram(sram) => sram.write(addr, data),
            Backup::Flash(flash) => flash.write(addr, data),
            _ => {
                warn!("Write GamePak RAM on non-SRAM cartridge: 0x{addr:08X} = 0x{data:02X}");
            }
        }
    }
}

fn match_id_string(s: &[u8], tag: &[u8]) -> bool {
    if s.len() < tag.len() {
        return false;
    }
    for i in 0..tag.len() {
        if tag[i] != b'n' {
            if s[i] != tag[i] {
                return false;
            }
        } else {
            if s[i] < b'0' || s[i] > b'9' {
                return false;
            }
        }
    }
    true
}
