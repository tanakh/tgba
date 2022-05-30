use log::{error, info, trace, warn};

pub struct Eeprom {
    data: Vec<u64>,
    state: EepromState,
}

#[derive(Debug)]
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

impl Eeprom {
    pub fn new(backup: Option<Vec<u8>>) -> Self {
        Self {
            data: backup.map_or_else(
                || vec![],
                |data| {
                    let data = data
                        .chunks(8)
                        .map(|c| u64::from_le_bytes(c.try_into().unwrap()))
                        .collect::<Vec<u64>>();
                    if matches!(data.len() * 8, 512 | 8192) {
                        error!(
                            "Invalid backup size: {:?}, expect 512bits or 8kbits",
                            data.len()
                        );
                    }
                    data
                },
            ),
            state: EepromState::WaitForCommand { step: 0 },
        }
    }

    pub fn size(&self) -> Option<EepromSize> {
        let bits = self.data.len() * 8;
        if bits == 512 {
            Some(EepromSize::Size512)
        } else if bits == 8192 {
            Some(EepromSize::Size8K)
        } else {
            None
        }
    }

    pub fn set_size(&mut self, size: EepromSize) {
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

    pub fn data(&self) -> Vec<u8> {
        self.data
            .iter()
            .flat_map(|x| x.to_le_bytes().to_vec())
            .collect()
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

    pub fn read(&mut self) -> bool {
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

        trace!("EEPROM: read {:?}", data as u8);
        data
    }

    pub fn write(&mut self, data: bool) {
        trace!("EEPROM: write {:?}", data as u8);

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
                        warn!("EEPROM: 0 must be written at the end of write command");
                    }
                    *ready = true;
                } else {
                    panic!("EEPROM: invalid write while read command");
                }
            }
            EepromState::Writing { addr, pos, buf } => {
                if *pos == 64 {
                    if data {
                        warn!("EEPROM: 0 must be written the end of write command");
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
