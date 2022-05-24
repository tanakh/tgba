use bitvec::prelude::*;

use crate::{
    context::{Interrupt, Sound, Timing},
    interrupt::InterruptKind,
    util::{pack, trait_alias},
};

trait_alias!(pub trait Context = Timing + Interrupt + Sound);

#[derive(Default)]
pub struct Timers {
    timer: [Timer; 4],
    prev_cycle: u64,
}

impl Timers {
    pub fn read16(&self, addr: u32) -> u16 {
        match addr {
            // TMxCNT_L
            0x100 | 0x104 | 0x108 | 0x10C => {
                let i = ((addr - 0x100) / 4) as usize;
                self.timer[i].counter
            }
            // TMxCNT_H
            0x102 | 0x106 | 0x10A | 0x10E => {
                let i = ((addr - 0x100) / 4) as usize;
                let timer = &self.timer[i];
                pack! {
                    0..=1 => timer.prescaler,
                    2     => timer.countup_timing,
                    6     => timer.irq_enable,
                    7     => timer.enable,
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn write16(&mut self, addr: u32, data: u16) {
        match addr {
            // TMxCNT_L
            0x100 | 0x104 | 0x108 | 0x10C => {
                let i = ((addr - 0x100) / 4) as usize;
                self.timer[i].reload = data;
            }
            // TMxCNT_H
            0x102 | 0x106 | 0x10A | 0x10E => {
                let i = ((addr - 0x100) / 4) as usize;
                let timer = &mut self.timer[i];
                let data = data.view_bits::<Lsb0>();
                timer.prescaler = data[0..=1].load();
                timer.countup_timing = data[2];
                timer.irq_enable = data[6];
                if !timer.enable && data[7] {
                    timer.counter = timer.reload;
                    timer.fraction = 0;
                }
                timer.enable = data[7];
            }
            _ => unreachable!(),
        }
    }

    pub fn tick(&mut self, ctx: &mut impl Context) {
        let prev_cycle = self.prev_cycle;
        let cur_cycle = ctx.now();
        let elapsed = cur_cycle - prev_cycle;
        self.prev_cycle = cur_cycle;

        let mut prev_overflow = 0;
        for ch in 0..4 {
            prev_overflow = self.timer[ch].process(ctx, ch, elapsed, prev_overflow);
        }
    }
}

#[derive(Default)]
struct Timer {
    enable: bool,
    counter: u16,
    reload: u16,
    irq_enable: bool,
    countup_timing: bool, // 0: prescaler, 1: prev timer overflow

    // 00: System clock
    // 01: System clock / 64
    // 10: System clock / 256
    // 11: System clock / 1024
    prescaler: u8,

    fraction: u64,
}

impl Timer {
    fn process(
        &mut self,
        ctx: &mut impl Context,
        ch: usize,
        elapsed: u64,
        prev_overflow: u64,
    ) -> u64 {
        if !self.enable {
            return 0;
        }

        let mut inc = if let Some(prescaler) = self.prescaler() {
            self.fraction += elapsed;
            let inc = self.fraction / prescaler;
            self.fraction %= prescaler;
            inc
        } else {
            prev_overflow
        };

        let mut overflow = 0;

        while inc > 0 {
            if self.counter as u64 + inc >= 0x10000 {
                overflow += 1;
                inc -= 0x10000 - self.counter as u64;
                self.counter = self.reload;

                if self.irq_enable {
                    let kind = match ch {
                        0 => InterruptKind::Timer0,
                        1 => InterruptKind::Timer1,
                        2 => InterruptKind::Timer2,
                        3 => InterruptKind::Timer3,
                        _ => unreachable!(),
                    };
                    ctx.interrupt_mut().set_interrupt(kind);
                }

                if ch == 0 || ch == 1 {
                    ctx.sound_timer_overflow(ch as _);
                }
            } else {
                self.counter += inc as u16;
                break;
            }
        }

        overflow
    }

    fn prescaler(&self) -> Option<u64> {
        if self.countup_timing {
            None
        } else {
            Some(match self.prescaler {
                0 => 1,
                1 => 64,
                2 => 256,
                3 => 1024,
                _ => unreachable!(),
            })
        }
    }
}
