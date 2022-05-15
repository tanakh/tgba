// #![feature(trace_macros)]

mod bus;
mod consts;
mod context;
mod cpu;
mod interrupt;
mod lcd;
mod rom;
mod sound;
mod util;

use context::Context;

pub use lcd::FrameBuf;
pub use rom::Rom;

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
        use context::Lcd;

        let start_frame = self.ctx.lcd().frame();
        while start_frame == self.ctx.lcd().frame() {
            self.ctx.cpu.exec_one(&mut self.ctx.inner);
            self.ctx.lcd_tick();
        }
    }

    pub fn frame_buf(&self) -> &FrameBuf {
        use context::Lcd;
        self.ctx.lcd().frame_buf()
    }
}
