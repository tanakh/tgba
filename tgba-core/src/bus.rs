use log::trace;

use crate::{context::Interrupt, rom::Rom, util::trait_alias};

trait_alias!(pub trait Context = Interrupt);

pub struct Bus {
    bios: Vec<u8>,
    rom: Rom,
    ram: Vec<u8>,
    ext_ram: Vec<u8>,
}

impl Bus {
    pub fn new(bios: Vec<u8>, rom: Rom) -> Self {
        let ram = vec![0; 0x8000];
        let ext_ram = vec![0; 0x40000];
        Bus {
            bios,
            rom,
            ram,
            ext_ram,
        }
    }

    pub fn read8(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        todo!()
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        todo!()
    }

    pub fn read32(&mut self, ctx: &mut impl Context, addr: u32) -> u32 {
        trace!("Read: {addr:08X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    read32(&self.bios, addr as usize)
                } else {
                    panic!()
                }
            }
            0x2 => read32(&self.ext_ram, (addr & 0x3FFFF) as usize),
            0x3 => read32(&self.ram, (addr & 0x7FFF) as usize),
            0x4 => {
                todo!("I/O")
            }
            0x5 => {
                todo!("Palette RAM")
            }
            0x6 => {
                todo!("VRAM")
            }
            0x7 => {
                todo!("OAM")
            }
            0x8..=0x9 => {
                todo!("GamePak Wait State 0")
            }
            0xA..=0xB => {
                todo!("GamePak Wait State 1")
            }
            0xC..=0xD => {
                todo!("GamePak Wait State 2")
            }
            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
        }
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        todo!()
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        todo!()
    }

    pub fn write32(&mut self, ctx: &mut impl Context, addr: u32, data: u32) {
        trace!("Read: {addr:08X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    write32(&mut self.bios, addr as usize, data)
                } else {
                    panic!()
                }
            }
            0x2 => write32(&mut self.ext_ram, (addr & 0x3FFFF) as usize, data),
            0x3 => write32(&mut self.ram, (addr & 0x7FFF) as usize, data),
            0x4 => {
                todo!("I/O")
            }
            0x5 => {
                todo!("Palette RAM")
            }
            0x6 => {
                todo!("VRAM")
            }
            0x7 => {
                todo!("OAM")
            }
            0x8..=0x9 => {
                todo!("GamePak Wait State 0")
            }
            0xA..=0xB => {
                todo!("GamePak Wait State 1")
            }
            0xC..=0xD => {
                todo!("GamePak Wait State 2")
            }
            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
        }
    }
}

fn read32(p: &[u8], addr: usize) -> u32 {
    u32::from_le_bytes(p[addr..addr + 4].try_into().unwrap())
}

fn write32(p: &mut [u8], addr: usize, data: u32) {
    p[addr..addr + 4].copy_from_slice(&data.to_le_bytes());
}
