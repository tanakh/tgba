use log::error;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Sram {
    #[serde(with = "serde_bytes")]
    data: Vec<u8>,
}

impl Sram {
    pub fn new(backup: Option<Vec<u8>>) -> Self {
        Self {
            data: backup.map_or_else(
                || vec![0; 0x10000],
                |data| {
                    if data.len() != 0x10000 {
                        error!("Invalid backup size: {:?}, expect 64KB", data.len());
                        vec![0; 0x10000]
                    } else {
                        data
                    }
                },
            ),
        }
    }

    pub fn data(&self) -> Vec<u8> {
        self.data.clone()
    }

    pub fn read(&self, addr: u32) -> u8 {
        self.data[addr as usize]
    }

    pub fn write(&mut self, addr: u32, data: u8) {
        self.data[addr as usize] = data;
    }
}
