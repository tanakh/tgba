use anyhow::{bail, Result};
use log::{info, warn};

pub struct Rom {
    pub data: Vec<u8>,
    pub title: String,
    pub game_code: [u8; 4],
    pub maker_code: [u8; 2],
    pub main_unit_code: u8,
    pub device_type: u8,
    pub rom_version: u8,
    pub backup: Option<Backup>,
}

pub enum Backup {
    // EEPROM_Vnnn    EEPROM 512 bytes or 8 Kbytes (4Kbit or 64Kbit)
    Eeprom(Eeprom),
    // SRAM_Vnnn      SRAM 32 Kbytes (256Kbit)
    Sram(Sram),
    // FLASH_Vnnn     FLASH 64 Kbytes (512Kbit) (ID used in older files)
    Flash(Flash),
    // FLASH512_Vnnn  FLASH 64 Kbytes (512Kbit) (ID used in newer files)
    Flash512(Flash512),
    // FLASH1M_Vnnn   FLASH 128 Kbytes (1Mbit)
    Flash1M(Flash1M),
}

pub struct Eeprom {
    data: Vec<u64>,
    state: EepromState,
}

pub enum EepromSize {
    Size512,
    Size8K,
}

pub enum EepromState {
    WaitForCommand { step: u32 },
    WaitForAddr { read: bool, addr: u32, pos: u32 },
    Reading { addr: u32, pos: u32, ready: bool },
    Writing { addr: u32, pos: u32, buf: u64 },
}

impl Default for Eeprom {
    fn default() -> Self {
        Self {
            data: vec![0; 512],
            state: EepromState::WaitForCommand { step: 0 },
        }
    }
}

impl Eeprom {
    fn size(&self) -> Option<EepromSize> {
        if self.data.len() == 512 / 8 {
            Some(EepromSize::Size512)
        } else if self.data.len() == 8 * 1024 / 8 {
            Some(EepromSize::Size8K)
        } else {
            None
        }
    }

    fn set_size(&mut self, size: EepromSize) {
        match size {
            EepromSize::Size512 => {
                info!("Setting EEPROM size to 512 bytes");
                self.data.resize(512 / 8, 0);
            }
            EepromSize::Size8K => {
                info!("Setting EEPROM size to 8K bytes");
                self.data.resize(8 * 1024 / 8, 0);
            }
        }
    }

    fn addr_len(&self) -> u32 {
        if self.data.len() == 512 / 8 {
            6
        } else if self.data.len() == 8 * 1024 / 8 {
            14
        } else {
            unreachable!()
        }
    }

    fn read(&mut self) -> bool {
        assert!(!self.data.is_empty());

        let data = match &mut self.state {
            EepromState::WaitForCommand { .. } => true,
            EepromState::WaitForAddr { .. } => false,
            EepromState::Reading { addr, pos, ready } => {
                if !*ready {
                    panic!("EEPROM: not ready for reading");
                }
                if *pos < 4 {
                    *pos += 1;
                    false
                } else {
                    let data = (self.data[*addr as usize] >> (4 + 63 - *pos)) & 1 != 0;
                    *pos += 1;
                    if *pos >= 64 + 4 {
                        info!("EEPROM: read command done");
                        self.state = EepromState::WaitForCommand { step: 0 };
                    }
                    data
                }
            }
            EepromState::Writing { .. } => false,
        };

        log::trace!("EEPROM: read {data:?}");
        data
    }

    fn write(&mut self, data: bool) {
        assert!(!self.data.is_empty());

        log::trace!("EEPROM: write {data:?}");

        let addr_len = self.addr_len();

        match &mut self.state {
            EepromState::WaitForCommand { step } => {
                if *step == 0 {
                    if data {
                        *step += 1;
                    }
                } else {
                    self.state = EepromState::WaitForAddr {
                        read: data,
                        addr: 0,
                        pos: 0,
                    };
                }
            }
            EepromState::WaitForAddr { read, addr, pos } => {
                *addr |= (data as u32) << *pos;
                *pos += 1;

                if *pos == addr_len {
                    let addr = if addr_len == 6 { *addr } else { *addr >> 4 };

                    self.state = if *read {
                        info!("EEPROM: read 0x{:03X}", addr);
                        EepromState::Reading {
                            addr,
                            pos: 0,
                            ready: false,
                        }
                    } else {
                        info!("EEPROM: write 0x{:03X}", addr);
                        EepromState::Writing {
                            addr,
                            pos: 0,
                            buf: 0,
                        }
                    };
                }
            }
            EepromState::Reading { ready, .. } => {
                if !*ready {
                    if data {
                        panic!("EEPROM: 0 must be written the end of write command");
                    }
                    *ready = true;
                } else {
                    panic!("EEPROM: invalid write while read command");
                }
            }
            EepromState::Writing { addr, pos, buf } => {
                if *pos == 64 {
                    if data {
                        panic!("EEPROM: 0 must be written the end of write command");
                    }
                    info!("EEPROM: write 0x{:03X} = 0x{:016X}", *addr, *buf);
                    self.data[*addr as usize] = *buf;
                    self.state = EepromState::WaitForCommand { step: 0 };
                } else {
                    *buf |= (data as u64) << (63 - *pos);
                    *pos += 1;
                }
            }
        }
    }
}

#[derive(Default)]
pub struct Sram {}

#[derive(Default)]
pub struct Flash {}

#[derive(Default)]
pub struct Flash512 {}

#[derive(Default)]
pub struct Flash1M {}

impl Backup {
    fn detect_backup(data: &[u8]) -> Option<Backup> {
        for i in 0..data.len() {
            if match_id_string(&data[i..], b"EEPROM_Vnnn") {
                return Some(Backup::Eeprom(Default::default()));
            }
            if match_id_string(&data[i..], b"SRAM_Vnnn") {
                return Some(Backup::Sram(Default::default()));
            }
            if match_id_string(&data[i..], b"FLASH_Vnnn") {
                return Some(Backup::Flash(Default::default()));
            }
            if match_id_string(&data[i..], b"FLASH512_Vnnn") {
                return Some(Backup::Flash512(Default::default()));
            }
            if match_id_string(&data[i..], b"FLASH1M_Vnnn") {
                return Some(Backup::Flash1M(Default::default()));
            }
        }
        None
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

impl Rom {
    pub fn from_bytes(data: &[u8]) -> Result<Self> {
        let header = &data[0xA0..0xC0];

        let title = String::from_utf8(header[..0xC].to_vec())?;
        let game_code = header[0xC..0x10].try_into()?;
        let maker_code = header[0x10..0x12].try_into()?;

        let magic = header[0x12];
        if magic != 0x96 {
            bail!("Invalid magic number: {magic:02X}");
        }

        let main_unit_code = header[0x13];
        let device_type = header[0x14];

        let rom_version = header[0x1C];
        let complement_check = header[0x1D];

        for i in (0x15..=0x1B).chain(0x1E..=0x1F) {
            if header[i] != 0 {
                warn!("Non-zero reserved area: {:02X}", header[i]);
            }
        }

        let sum = header[..=0x1C]
            .iter()
            .fold(0_u8, |a, b| a.wrapping_add(*b))
            .wrapping_add(0x19);

        if sum.wrapping_add(complement_check) != 0 {
            warn!("Invalid complement check: {sum:02X} + {complement_check:02X} != 0");
        }

        let ret = Rom {
            data: data.to_vec(),
            title,
            game_code,
            maker_code,
            main_unit_code,
            device_type,
            rom_version,
            backup: Backup::detect_backup(&data),
        };

        Ok(ret)
    }

    pub fn backup_type(&self) -> &str {
        match &self.backup {
            Some(backup) => match backup {
                Backup::Eeprom(_) => "EEPROM",
                Backup::Sram(_) => "SRAM",
                Backup::Flash(_) => "FLASH",
                Backup::Flash512(_) => "FLASH512",
                Backup::Flash1M(_) => "FLASH1M",
            },
            None => "Unknown",
        }
    }

    pub fn eeprom_size(&self) -> Option<Option<EepromSize>> {
        if let Some(Backup::Eeprom(eeprom)) = &self.backup {
            Some(eeprom.size())
        } else {
            None
        }
    }

    pub fn set_eeprom_size(&mut self, size: EepromSize) {
        if let Some(Backup::Eeprom(eeprom)) = &mut self.backup {
            eeprom.set_size(size);
        } else {
            warn!("This cartridge has no EEPROM");
        }
    }

    pub fn read_eeprom(&mut self) -> bool {
        if let Some(Backup::Eeprom(eeprom)) = &mut self.backup {
            eeprom.read()
        } else {
            warn!("This cartridge has no EEPROM");
            false
        }
    }

    pub fn write_eeprom(&mut self, data: bool) {
        if let Some(Backup::Eeprom(eeprom)) = &mut self.backup {
            eeprom.write(data);
        } else {
            warn!("This cartridge has no EEPROM");
        }
    }
}
