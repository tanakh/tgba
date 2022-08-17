use log::{debug, error, trace, warn};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Eeprom {
    state: EepromState,
    data: Vec<u64>,
}

#[derive(Debug)]
pub enum EepromSize {
    Size512,
    Size8K,
}

#[derive(Serialize, Deserialize)]
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
                    if !matches!(data.len() * 8, 512 | 8192) {
                        error!("Invalid backup size: {:?}, expect 512B or 8KiB", data.len());
                    }
                    data
                },
            ),
            state: EepromState::WaitForCommand { step: 0 },
        }
    }

    // pub fn size(&self) -> Option<EepromSize> {
    //     let bits = self.data.len() * 8;
    //     if bits == 512 {
    //         Some(EepromSize::Size512)
    //     } else if bits == 8192 {
    //         Some(EepromSize::Size8K)
    //     } else {
    //         None
    //     }
    // }

    pub fn set_size(&mut self, size: EepromSize) {
        let words = match size {
            EepromSize::Size512 => 512 / 8,
            EepromSize::Size8K => 8192 / 8,
        };

        if self.data.is_empty() {
            debug!("Setting EEPROM size to {} bytes", words * 8);
        } else if self.data.len() != words as usize {
            warn!(
                "Different EEPROM size detected: {} bytes, previous = {} bytes",
                words * 8,
                self.data.len() * 8
            );
        }

        self.data.resize(words, 0);
    }

    pub fn data(&self) -> Vec<u8> {
        self.data
            .iter()
            .flat_map(|x| x.to_le_bytes().to_vec())
            .collect()
    }

    fn addr_len(&self) -> Option<u32> {
        if self.data.len() == 512 / 8 {
            Some(6)
        } else if self.data.len() == 8 * 1024 / 8 {
            Some(14)
        } else {
            None
        }
    }

    pub fn read(&mut self) -> bool {
        let data = match &mut self.state {
            EepromState::WaitForCommand { .. } => true,
            EepromState::WaitForAddr { .. } => false,
            EepromState::Reading { addr, pos, ready } => {
                if !*ready {
                    panic!("Not ready for reading");
                }
                if *pos < 4 {
                    *pos += 1;
                    false
                } else {
                    let data = (self.data[*addr as usize] >> (4 + 63 - *pos)) & 1 != 0;
                    *pos += 1;
                    if *pos >= 64 + 4 {
                        debug!("Read command done");
                        self.state = EepromState::WaitForCommand { step: 0 };
                    }
                    data
                }
            }
            EepromState::Writing { .. } => false,
        };

        trace!("Read {:?}", data as u8);
        data
    }

    pub fn write(&mut self, data: bool) {
        trace!("Write {:?}", data as u8);

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

                let addr_len = addr_len.unwrap();

                if *pos == addr_len {
                    let addr = if addr_len == 6 { *addr } else { *addr >> 4 };

                    self.state = if *read {
                        debug!("Read 0x{:03X}", addr);
                        EepromState::Reading {
                            addr,
                            pos: 0,
                            ready: false,
                        }
                    } else {
                        debug!("Write 0x{:03X}", addr);
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
                        warn!("0 must be written at the end of read command");
                    }
                    *ready = true;
                } else {
                    panic!("Invalid write while read command");
                }
            }
            EepromState::Writing { addr, pos, buf } => {
                if *pos == 64 {
                    if data {
                        warn!("0 must be written at the end of write command");
                    }
                    debug!("Write 0x{:03X} = 0x{:016X}", *addr, *buf);
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
