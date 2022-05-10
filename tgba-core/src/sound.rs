use log::{info, warn};

use crate::{context::Interrupt, util::trait_alias};

pub struct Sound {}

trait_alias!(pub trait Context = Interrupt);

impl Sound {
    pub fn new() -> Sound {
        Sound {}
    }

    pub fn read8(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        info!("Sound read8: 0x{addr:08X}");
        0
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        match addr {
            // SOUNDBIAS
            0x088 => 0x0200,

            _ => {
                warn!("Sound read16: 0x{addr:08X}");
                0
            }
        }
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        info!("Sound write8: 0x{addr:08X} = 0x{data:02X}");
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        info!("Sound write16: 0x{addr:08X} = 0x{data:04X}");
    }
}
