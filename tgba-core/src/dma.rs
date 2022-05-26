use bitvec::prelude::*;
use log::{log_enabled, trace, warn};

use crate::{
    backup::eeprom::EepromSize,
    bus::Bus,
    context::{Interrupt, Lcd, Sound, SoundDma, Timing},
    interrupt::InterruptKind,
    util::{enum_pat, pack, trait_alias, ConstEval},
};

trait_alias!(pub trait Context = Lcd + Sound + SoundDma + Interrupt + Timing);

#[derive(Debug)]
pub struct Dma {
    ch: usize,

    src_addr: u32,
    dest_addr: u32,
    word_count: u32,

    src_addr_ctrl: u8,
    dest_addr_ctrl: u8,
    repeat: bool,
    transfer_type: bool, // 0: 16bit, 1: 32bit
    game_pak_data_request_transfer: bool,

    // 00: Start immediately
    // 01: Start in a V-blank interval
    // 10: Start in an H-blank interval
    // 11: Special
    start_timing: u8,
    irq_enable: bool,
    dma_enable: bool,

    wait_for_exec: bool,
    src_addr_internal: u32,
    dest_addr_internal: u32,
    word_count_internal: u32,

    prev_dma_frame: u64,
    prev_dma_line: u32,
}

enum StartTiming {
    Immediately = 0,
    VBlank = 1,
    HBlank = 2,
    Special = 3,
}

impl Dma {
    pub fn new(ch: usize) -> Self {
        Self {
            ch,
            src_addr: 0,
            dest_addr: 0,
            word_count: 0,
            src_addr_ctrl: 0,
            dest_addr_ctrl: 0,
            repeat: false,
            transfer_type: false,
            game_pak_data_request_transfer: false,
            start_timing: 0,
            irq_enable: false,
            dma_enable: false,

            wait_for_exec: false,
            src_addr_internal: 0,
            dest_addr_internal: 0,
            word_count_internal: 0,

            prev_dma_frame: 0,
            prev_dma_line: 0,
        }
    }

    fn repeat(&self) -> bool {
        self.repeat
            && (self.start_timing == StartTiming::VBlank as u8
                || self.start_timing == StartTiming::HBlank as u8
                || self.start_timing == StartTiming::Special as u8)
    }

    fn check_dma_start(&mut self, ctx: &mut impl Context) -> bool {
        match self.start_timing {
            enum_pat!(StartTiming::Immediately) => self.wait_for_exec,
            // Start in a V-blank interval
            enum_pat!(StartTiming::VBlank) => {
                self.prev_dma_frame != ctx.lcd().frame() && ctx.lcd().vblank()
            }
            // Start in an H-blank interval
            enum_pat!(StartTiming::HBlank) => {
                let lcd = ctx.lcd();
                lcd.hblank()
                    && (self.prev_dma_frame, self.prev_dma_line) != (lcd.frame(), lcd.line())
            }
            enum_pat!(StartTiming::Special) => match self.ch {
                // Prohibited
                0 => false,
                1 | 2 => {
                    let ret = ctx.sound_dma_request(self.ch as u8 - 1);
                    ctx.set_sound_dma_request(self.ch as u8 - 1, false);
                    ret
                }
                3 => {
                    // TODO: Video capture
                    false
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn is_sound_dma(&self) -> bool {
        (self.ch == 1 || self.ch == 2) && self.start_timing == StartTiming::Special as u8
    }

    fn trace(&self, ctx: &mut impl Context, src: &[u8]) {
        trace!("DMA{}: cycle: {}", self.ch, ctx.now());
        trace!("  - enable: {}", if self.dma_enable { "yes" } else { "no" });

        trace!(
            "  - src:    0x{:08X} (0x{:08X}) {} = {src:02X?}",
            self.src_addr_internal,
            self.src_addr,
            match self.src_addr_ctrl {
                0 => "inc",
                1 => "dec",
                2 => "fixed",
                3 => "prohibited",
                _ => unreachable!(),
            }
        );
        trace!(
            "  - dest:   0x{:08X} (0x{:08X}) {}",
            self.dest_addr_internal,
            self.dest_addr,
            match self.dest_addr_ctrl {
                0 => "inc",
                1 => "dec",
                2 => "fixed",
                3 => "inc/reload",
                _ => unreachable!(),
            }
        );
        trace!(
            "  - count:  0x{:X} * {}byte",
            self.word_count,
            if self.transfer_type { 4 } else { 2 }
        );
        trace!("  - repeat: {}", if self.repeat { "yes" } else { "no" });
        if self.ch == 3 {
            trace!(
                "  - game pak DRQ: {}",
                if !self.game_pak_data_request_transfer {
                    "normal"
                } else {
                    "DRQ <from> Game Pak"
                }
            );
        }
        trace!(
            "  - start:  {}",
            match self.start_timing {
                0 => "immediately",
                1 => "VBlank",
                2 => "HBlank",
                3 => match self.ch {
                    0 => "prohibited",
                    1 | 2 => "Sound FIFO",
                    3 => "Video capture",
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        );
        trace!("  - IRQ:    {}", if self.irq_enable { "yes" } else { "no" });
    }
}

impl Bus {
    pub fn process_dma(&mut self, ctx: &mut impl Context, ch: usize) {
        if !self.dma(ch).dma_enable {
            return;
        }

        if !self.dma_mut(ch).check_dma_start(ctx) {
            return;
        }

        // The CPU is paused when DMA transfers are active, however, the CPU is operating during the periods when Sound/Blanking DMA transfers are paused.

        // Transfer Rate/Timing
        // Except for the first data unit, all units are transferred by sequential reads and writes. For n data units, the DMA transfer time is:

        //   2N+2(n-1)S+xI
        // Of which, 1N+(n-1)S are read cycles, and the other 1N+(n-1)S are write cycles, actual number of cycles depends on the waitstates and bus-width of the source and destination areas (as described in CPU Instruction Cycle Times chapter). Internal time for DMA processing is 2I (normally), or 4I (if both source and destination are in gamepak memory area).

        self.dma_mut(ch).prev_dma_frame = ctx.lcd().frame();
        self.dma_mut(ch).prev_dma_line = ctx.lcd().line();

        let is_sound_dma = self.dma(ch).is_sound_dma();

        let word_len = if self.dma(ch).transfer_type || is_sound_dma {
            4
        } else {
            2
        };

        let word_count = if is_sound_dma {
            4
        } else {
            self.dma(ch).word_count
        };

        if ch == 3
            && self.is_valid_eeprom_addr(self.dma(ch).dest_addr_internal)
            && word_len == 2
            && matches!(self.backup().eeprom_size(), Some(None))
        {
            match word_count {
                9 => self.backup_mut().set_eeprom_size(EepromSize::Size512),
                17 => self.backup_mut().set_eeprom_size(EepromSize::Size8K),
                _ => warn!("Write EEPROM to unknown size with DMA: {}", word_count),
            }
        }

        let src_inc = match self.dma(ch).src_addr_ctrl {
            // increment
            0 => word_len,
            // decrement
            1 => -(word_len as i32) as u32,
            // fixed
            2 => 0,
            // prohibited
            3 => panic!(),
            _ => unreachable!(),
        };

        let dest_inc = if is_sound_dma {
            0
        } else {
            match self.dma(ch).dest_addr_ctrl {
                // increment
                0 => word_len,
                // decrement
                1 => -(word_len as i32) as u32,
                // fixed
                2 => 0,
                // increment / reload
                3 => word_len,
                _ => unreachable!(),
            }
        };

        if log_enabled!(log::Level::Trace) {
            let src = if ch == 1 && self.dma(ch).start_timing == 3 {
                (0..16)
                    .map(|i| {
                        let addr = self.dma(ch).src_addr_internal;
                        self.ram[(addr + i) as usize & 0x7FFF]
                    })
                    .collect()
            } else {
                vec![]
            };

            self.dma(ch).trace(ctx, &src);
        }

        for i in 0..word_count {
            if word_len == 4 {
                if self.dma(ch).src_addr_internal % 4 != 0 {
                    warn!(
                        "DMA src addr misaligned: {}",
                        self.dma(ch).src_addr_internal
                    );
                }
                if self.dma(ch).dest_addr_internal % 4 != 0 {
                    warn!(
                        "DMA dest addr misaligned: {}",
                        self.dma(ch).dest_addr_internal
                    );
                }

                let data = self.read32(ctx, self.dma(ch).src_addr_internal & !3, i == 0);
                self.write32(ctx, self.dma(ch).dest_addr_internal & !3, data, i == 0);
            } else {
                if self.dma(ch).src_addr_internal % 2 != 0 {
                    warn!(
                        "DMA src addr misaligned: {}",
                        self.dma(ch).src_addr_internal
                    );
                }
                if self.dma(ch).dest_addr_internal % 2 != 0 {
                    warn!(
                        "DMA dest addr misaligned: {}",
                        self.dma(ch).dest_addr_internal
                    );
                }

                let data = self.read16(ctx, self.dma(ch).src_addr_internal & !1, i == 0);
                self.write16(ctx, self.dma(ch).dest_addr_internal & !1, data, i == 0);
            }

            self.dma_mut(ch).src_addr_internal =
                self.dma(ch).src_addr_internal.wrapping_add(src_inc);
            self.dma_mut(ch).dest_addr_internal =
                self.dma(ch).dest_addr_internal.wrapping_add(dest_inc);
        }

        self.dma_mut(ch).wait_for_exec = false;

        if !self.dma(ch).repeat() {
            self.dma_mut(ch).dma_enable = false;
        } else if self.dma(ch).dest_addr_ctrl == 3 {
            self.dma_mut(ch).dest_addr_internal = self.dma(ch).dest_addr;
        }

        if self.dma(ch).irq_enable {
            let kind = match ch {
                0 => InterruptKind::Dma0,
                1 => InterruptKind::Dma1,
                2 => InterruptKind::Dma2,
                3 => InterruptKind::Dma3,
                _ => unreachable!(),
            };
            ctx.interrupt_mut().set_interrupt(kind);
        }
    }

    pub fn read_dma16(&self, addr: u32) -> u16 {
        match addr {
            // DMAxSAD
            0x0B0 | 0x0BC | 0x0C8 | 0x0D4 => 0,
            0x0B2 | 0x0BE | 0x0CA | 0x0D6 => 0,

            // DMAxDAD
            0x0B4 | 0x0C0 | 0x0CC | 0x0D8 => 0,
            0x0B6 | 0x0C2 | 0x0CE | 0x0DA => 0,

            // DMAxCNT
            0x0B8 | 0x0C4 | 0x0D0 | 0x0DC => 0,
            0x0BA | 0x0C6 | 0x0D2 | 0x0DE => {
                let ch = ((addr - 0x0B0) / 0xC) as usize;
                let dma = self.dma(ch);
                pack! {
                    5..=6   => dma.dest_addr_ctrl,
                    7..=8   => dma.src_addr_ctrl,
                    9       => dma.repeat,
                    10      => dma.transfer_type,
                    11      => dma.game_pak_data_request_transfer,
                    12..=13 => dma.start_timing,
                    14      => dma.irq_enable,
                    15      => dma.dma_enable,
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn write_dma16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        match addr {
            // DMAxSAD
            0x0B0 | 0x0BC | 0x0C8 | 0x0D4 => {
                let ch = ((addr - 0x0B0) / 0xC) as usize;
                self.dma_mut(ch).src_addr.view_bits_mut::<Lsb0>()[0..=15].store(data);
            }
            0x0B2 | 0x0BE | 0x0CA | 0x0D6 => {
                let ch = ((addr - 0x0B0) / 0xC) as usize;
                let hi = if ch == 0 { 26 } else { 27 };
                self.dma_mut(ch).src_addr.view_bits_mut::<Lsb0>()[16..=hi].store(data);
            }

            // DMAxDAD
            0x0B4 | 0x0C0 | 0x0CC | 0x0D8 => {
                let ch = ((addr - 0x0B0) / 0xC) as usize;
                self.dma_mut(ch).dest_addr.view_bits_mut::<Lsb0>()[0..=15].store(data);
            }
            0x0B6 | 0x0C2 | 0x0CE | 0x0DA => {
                let ch = ((addr - 0x0B0) / 0xC) as usize;
                let hi = if ch != 3 { 26 } else { 27 };
                self.dma_mut(ch).dest_addr.view_bits_mut::<Lsb0>()[16..=hi].store(data);
            }

            // DMAxCNT
            0x0B8 | 0x0C4 | 0x0D0 | 0x0DC => {
                let ch = ((addr - 0x0B0) / 0xC) as usize;
                let mask = if ch != 3 { 0x3FFF } else { 0xFFFF };
                let data = (data & mask) as u32;
                self.dma_mut(ch).word_count = if data == 0 { mask as u32 + 1 } else { data };
            }
            0x0BA | 0x0C6 | 0x0D2 | 0x0DE => {
                let ch = ((addr - 0x0B0) / 0xC) as usize;
                let dma = self.dma_mut(ch);

                let v = data.view_bits::<Lsb0>();
                dma.dest_addr_ctrl = v[5..=6].load();
                dma.src_addr_ctrl = v[7..=8].load();
                dma.repeat = v[9];
                dma.transfer_type = v[10];
                if ch == 3 {
                    dma.game_pak_data_request_transfer = v[11];
                }
                dma.start_timing = v[12..=13].load();
                dma.irq_enable = v[14];

                if !dma.dma_enable && v[15] {
                    // Reload src addr, dest addr and word count
                    dma.wait_for_exec = true;
                    dma.src_addr_internal = dma.src_addr;
                    dma.dest_addr_internal = dma.dest_addr;
                    dma.word_count_internal = dma.word_count;
                }

                dma.dma_enable = v[15];
            }
            _ => unreachable!(),
        }
    }
}
