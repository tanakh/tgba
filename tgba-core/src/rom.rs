use anyhow::{bail, Result};
use log::warn;

use crate::backup::Backup;

pub struct Rom {
    pub data: Vec<u8>,
    pub title: Vec<u8>,
    pub game_code: [u8; 4],
    pub maker_code: [u8; 2],
    pub main_unit_code: u8,
    pub device_type: u8,
    pub rom_version: u8,
}

impl Rom {
    pub fn from_bytes(data: &[u8]) -> Result<Self> {
        let header = &data[0xA0..0xC0];

        let title = header[..0xC].to_vec();
        let game_code = header[0xC..0x10].try_into()?;
        let maker_code = header[0x10..0x12].try_into()?;

        let magic = header[0x12];
        if magic != 0x96 {
            bail!("Invalid magic number: 0x{magic:02X}, expected: 0x96");
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
        };

        Ok(ret)
    }

    pub fn backup_type(&self) -> &'static str {
        Backup::detect_backup(&self.data, None).backup_type()
    }
}
