mod bus;
mod context;
mod cpu;
mod rom;
mod util;

use context::Context;

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
        loop {
            self.ctx.cpu.exec_one(&mut self.ctx.inner);
        }
    }
}
