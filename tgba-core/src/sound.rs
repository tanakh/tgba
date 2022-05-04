use crate::{context::Interrupt, util::trait_alias};

pub struct Sound {}

trait_alias!(pub trait Context = Interrupt);

impl Sound {
    pub fn new() -> Sound {
        Sound {}
    }

    pub fn read8(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        todo!()
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        todo!()
    }

    pub fn read32(&mut self, ctx: &mut impl Context, addr: u32) -> u32 {
        todo!()
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        todo!()
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        todo!()
    }

    pub fn write32(&mut self, ctx: &mut impl Context, addr: u32, data: u32) {
        todo!()
    }
}
