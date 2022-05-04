use bitvec::prelude::*;

use crate::{context::Interrupt, util::trait_alias};

#[derive(Default)]
pub struct Lcd {
    bg_mode: u8,
    display_frame_select: bool,
    hblank_obj_process: bool,
    obj_format: bool,
    force_blank: bool,
    display_bg: [bool; 4],
    display_obj: bool,
    display_window: [bool; 2],
    display_obj_window: bool,
}

trait_alias!(pub trait Context = Interrupt);

impl Lcd {
    pub fn new() -> Lcd {
        Lcd::default()
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
        match addr {
            // DISPCNT
            0x000 => {
                let v = data.view_bits::<Lsb0>();
                self.bg_mode = v[0..=2].load();
                self.display_frame_select = v[4];
                self.hblank_obj_process = v[5];
                self.obj_format = v[6];
                self.force_blank = v[7];
                self.display_bg[0] = v[8];
                self.display_bg[1] = v[9];
                self.display_bg[2] = v[10];
                self.display_bg[3] = v[11];
                self.display_obj = v[12];
                self.display_window[0] = v[13];
                self.display_window[1] = v[14];
                self.display_obj_window = v[15];
            }

            _ => todo!(),
        }
    }

    pub fn write32(&mut self, ctx: &mut impl Context, addr: u32, data: u32) {
        todo!()
    }
}
