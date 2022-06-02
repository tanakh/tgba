use log::{debug, error, trace, warn};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Flash {
    state: State,
    read_mode: ReadMode,
    bank: u32,
    #[serde(with = "serde_bytes")]
    data: Vec<u8>,
}

#[derive(Debug, Serialize, Deserialize)]
enum State {
    WaitForCommand(usize, CommandContext),
    WriteSingleByte,
    BankChange,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
enum CommandContext {
    None,
    Erase,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
enum ReadMode {
    Data,
    ChipId,
}

impl Flash {
    pub fn new(size: usize, backup: Option<Vec<u8>>) -> Self {
        let data = if let Some(backup) = backup {
            if backup.len() != size {
                error!(
                    "Invalid backup size: {:?}, expect {} bytes",
                    backup.len(),
                    size
                );
                vec![0xFF; size]
            } else {
                backup
            }
        } else {
            vec![0xFF; size]
        };

        Self {
            state: State::WaitForCommand(0, CommandContext::None),
            read_mode: ReadMode::Data,
            bank: 0,
            data,
        }
    }

    pub fn backup_type(&self) -> &'static str {
        if self.data.len() == 64 * 1024 {
            "FLASH (512K)"
        } else if self.data.len() == 128 * 1024 {
            "FLASH (1M)"
        } else {
            unreachable!()
        }
    }

    pub fn data(&self) -> Vec<u8> {
        self.data.clone()
    }

    pub fn read(&mut self, addr: u32) -> u8 {
        let addr = addr & 0xFFFF;
        match &mut self.read_mode {
            ReadMode::ChipId => {
                // ID     Name       Size  Sectors  AverageTimings  Timeouts/ms   Waits
                // D4BFh  SST        64K   16x4K    20us?,?,?       10,  40, 200  3,2
                // 1CC2h  Macronix   64K   16x4K    ?,?,?           10,2000,2000  8,3
                // 1B32h  Panasonic  64K   16x4K    ?,?,?           10, 500, 500  4,2
                // 3D1Fh  Atmel      64K   512x128  ?,?,?           ...40..,  40  8,8
                // 1362h  Sanyo      128K  ?        ?,?,?           ?    ?    ?    ?
                // 09C2h  Macronix   128K  ?        ?,?,?           ?    ?    ?    ?

                if self.data.len() == 64 * 1024 {
                    // Emulate SST for 64KB Flash
                    match addr {
                        0x0000 => 0xBF,
                        0x0001 => 0xD4,
                        _ => 0,
                    }
                } else {
                    // Emulate Sanyo for 128KB Flash
                    match addr {
                        0x0000 => 0x62,
                        0x0001 => 0x13,
                        _ => 0,
                    }
                }
            }
            ReadMode::Data => self.data[self.bank as usize * 0x10000 + (addr as usize & 0xFFFF)],
        }
    }

    pub fn write(&mut self, addr: u32, data: u8) {
        let addr = addr & 0xFFFF;

        trace!("Write Flash: 0x{addr:04X} = 0x{data:02X}");

        match &mut self.state {
            State::WaitForCommand(step, ctx) => match (*step, addr, data) {
                (0, 0x5555, 0xAA) => *step = 1,
                (1, 0x2AAA, 0x55) => *step = 2,

                (2, 0x5555, 0x90) if *ctx == CommandContext::None => {
                    debug!("Enter ID mode");
                    self.read_mode = ReadMode::ChipId;
                    self.state = State::WaitForCommand(0, CommandContext::None);
                }
                (2, 0x5555, 0xF0) if *ctx == CommandContext::None => {
                    debug!("Terminate ID mode");
                    if self.read_mode != ReadMode::ChipId {
                        panic!("FLASH: leave ID mode without entering");
                    }
                    self.read_mode = ReadMode::Data;
                    self.state = State::WaitForCommand(0, CommandContext::None);
                }

                (2, 0x5555, 0x80) => {
                    debug!("Enter erase mode");
                    self.state = State::WaitForCommand(0, CommandContext::Erase);
                }
                (2, 0x5555, 0x10) if *ctx == CommandContext::Erase => {
                    debug!("Erase entire chip");
                    self.data.fill(0xFF);
                    self.state = State::WaitForCommand(0, CommandContext::None);
                }
                (2, _, 0x30) if *ctx == CommandContext::Erase => {
                    let sector = (addr >> 12) as usize;
                    debug!("Erase sector {sector}");
                    let start = self.bank as usize * 0x10000 + sector * 0x1000;
                    self.data[start..start + 0x1000].fill(0xFF);
                    self.state = State::WaitForCommand(0, CommandContext::None);
                }

                (2, 0x5555, 0xA0) => {
                    trace!("Write single byte");
                    self.state = State::WriteSingleByte;
                }

                (2, 0x5555, 0xB0) => {
                    trace!("Enter bank change");
                    self.state = State::BankChange;
                }

                _ => {
                    warn!("Invalid command: data=0x{data:02X}, state:{:?}", self.state);
                }
            },

            State::WriteSingleByte => {
                // Only 1 -> 0 write is possible
                let addr = self.bank as usize * 0x10000 + (addr as usize & 0xFFFF);
                self.data[addr] &= data;
                debug!("Write single byte: 0x{addr:05X} = 0x{data:02X}");
                self.state = State::WaitForCommand(0, CommandContext::None);
            }

            State::BankChange => {
                assert_eq!(addr, 0);
                assert!((data as usize) < self.data.len() / (64 * 1024));
                debug!("Bank change: {data}");
                self.bank = data as u32;
                self.state = State::WaitForCommand(0, CommandContext::None);
            }
        }
    }
}
