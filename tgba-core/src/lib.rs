// #![feature(trace_macros)]

mod bus;
mod consts;
mod context;
mod cpu;
mod interface;
mod interrupt;
mod lcd;
mod rom;
mod sound;
mod timer;
mod util;

use context::Context;

use interface::AudioBuf;
pub use interface::{FrameBuf, KeyInput};
pub use rom::Rom;

use crate::context::Bus;

pub struct Agb {
    ctx: Context,
}

impl Agb {
    pub fn new(bios: Vec<u8>, rom: Rom) -> Self {
        Agb {
            ctx: Context::new(bios, rom),
        }
    }

    pub fn run_frame(&mut self) {
        use context::{Lcd, Sound};

        self.ctx.sound_mut().clear_buf();

        let start_frame = self.ctx.lcd().frame();
        while start_frame == self.ctx.lcd().frame() {
            self.ctx.cpu.exec_one(&mut self.ctx.inner);
            self.ctx.lcd_tick();
            self.ctx.sound_tick();
            self.ctx.bus_tick();
        }
    }

    pub fn frame_buf(&self) -> &FrameBuf {
        use context::Lcd;
        self.ctx.lcd().frame_buf()
    }

    pub fn audio_buf(&self) -> &AudioBuf {
        use context::Sound;
        self.ctx.sound().audio_buf()
    }

    pub fn set_key_input(&mut self, key_input: &KeyInput) {
        self.ctx.bus_mut().set_key_input(key_input);
    }
}
