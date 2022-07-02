use bitvec::prelude::*;
use log::{log_enabled, trace, warn};
use serde::{Deserialize, Serialize};

use crate::{
    backup::eeprom::EepromSize,
    bus::Bus,
    consts::{HBLANK_POS, SCREEN_HEIGHT},
    context::{GamePak, Interrupt, Lcd, Sound, SoundDma, Timing},
    interrupt::InterruptKind,
    util::{enum_pat, pack, trait_alias, ConstEval},
};

trait_alias!(pub trait Context = Lcd + Sound + SoundDma + GamePak + Interrupt + Timing);

#[derive(Debug, Serialize, Deserialize)]
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

    start_timing: u8,
    irq_enable: bool,
    enable: bool,

    running: bool,
    src_addr_internal: u32,
    dest_addr_internal: u32,
    word_count_internal: u32,
    word_len_internal: u32,
    first_access: bool,
    src_inc: u32,
    dest_inc: u32,

    prev_dma_frame: u64,
    prev_dma_line: u32,
}

// 00: Start immediately
// 01: Start in a V-blank interval
// 10: Start in an H-blank interval
// 11: Special

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
            enable: false,

            running: false,
            src_addr_internal: 0,
            dest_addr_internal: 0,
            word_count_internal: 0,
            word_len_internal: 0,
            first_access: false,
            src_inc: 0,
            dest_inc: 0,

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
            enum_pat!(StartTiming::Immediately) => false,
            // Start in a V-blank interval
            enum_pat!(StartTiming::VBlank) => {
                self.prev_dma_frame != ctx.lcd().frame() && ctx.lcd().line() >= SCREEN_HEIGHT
            }
            // Start in an H-blank interval
            enum_pat!(StartTiming::HBlank) => {
                let lcd = ctx.lcd();
                lcd.x() >= HBLANK_POS
                    && lcd.line() < SCREEN_HEIGHT
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

    fn trace(&self) {
        if self.ch != 3 {
            return;
        }

        trace!("DMA{}:", self.ch);
        trace!("  - enable: {}", if self.enable { "yes" } else { "no" });

        trace!(
            "  - src:    0x{:08X} (0x{:08X}) {}",
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

    pub fn read16(&self, addr: u32) -> u16 {
        match addr {
            // DMAxSAD
            0 | 2 => 0,

            // DMAxDAD
            4 | 6 => 0,

            // DMAxCNT
            8 => 0,
            10 => pack! {
                5..=6   => self.dest_addr_ctrl,
                7..=8   => self.src_addr_ctrl,
                9       => self.repeat,
                10      => self.transfer_type,
                11      => self.game_pak_data_request_transfer,
                12..=13 => self.start_timing,
                14      => self.irq_enable,
                15      => self.enable,
            },
            _ => unreachable!(),
        }
    }

    pub fn write16<C: GamePak + Lcd>(&mut self, ctx: &mut C, addr: u32, data: u16) {
        match addr {
            // DMAxSAD
            0 => self.src_addr.view_bits_mut::<Lsb0>()[0..=15].store(data),
            2 => {
                let hi = if self.ch == 0 { 26 } else { 27 };
                self.src_addr.view_bits_mut::<Lsb0>()[16..=hi].store(data);
            }

            // DMAxDAD
            4 => self.dest_addr.view_bits_mut::<Lsb0>()[0..=15].store(data),
            6 => {
                let hi = if self.ch != 3 { 26 } else { 27 };
                self.dest_addr.view_bits_mut::<Lsb0>()[16..=hi].store(data);
            }

            // DMAxCNT
            8 => {
                let mask = if self.ch != 3 { 0x3FFF } else { 0xFFFF };
                let data = (data & mask) as u32;
                self.word_count = if data == 0 { mask as u32 + 1 } else { data };
            }
            10 => {
                let v = data.view_bits::<Lsb0>();
                self.dest_addr_ctrl = v[5..=6].load();
                self.src_addr_ctrl = v[7..=8].load();
                self.repeat = v[9];
                self.transfer_type = v[10];
                if self.ch == 3 {
                    self.game_pak_data_request_transfer = v[11];
                }
                self.start_timing = v[12..=13].load();
                self.irq_enable = v[14];
                let old_enable = self.enable;
                self.enable = v[15];

                if !old_enable && self.enable {
                    // Reload src addr, dest addr and word count
                    self.src_addr_internal = self.src_addr;
                    self.dest_addr_internal = self.dest_addr;
                    self.word_count_internal = self.word_count;

                    if self.start_timing == StartTiming::Immediately as u8 {
                        self.start(ctx);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn start<C: GamePak + Lcd>(&mut self, ctx: &mut C) {
        self.running = true;

        self.prev_dma_frame = ctx.lcd().frame();
        self.prev_dma_line = ctx.lcd().line();

        self.word_len_internal = if self.transfer_type { 4 } else { 2 };
        self.word_count_internal = self.word_count;
        self.dest_inc = match self.dest_addr_ctrl {
            // increment
            0 => self.word_len_internal,
            // decrement
            1 => -(self.word_len_internal as i32) as u32,
            // fixed
            2 => 0,
            // increment / reload
            3 => self.word_len_internal,
            _ => unreachable!(),
        };

        self.src_inc = match self.src_addr_ctrl {
            // increment
            0 => self.word_len_internal,
            // decrement
            1 => -(self.word_len_internal as i32) as u32,
            // fixed
            2 => 0,
            // prohibited
            3 => panic!(),
            _ => unreachable!(),
        };

        if self.is_sound_dma() {
            self.word_len_internal = 4;
            self.word_count_internal = 4;
            self.dest_inc = 0;
        }

        self.first_access = true;

        if self.ch == 3
            && ctx.gamepak().is_valid_eeprom_addr(self.dest_addr_internal)
            && self.word_len_internal == 2
        {
            match self.word_count_internal {
                9 => ctx.backup_mut().set_eeprom_size(EepromSize::Size512),
                17 => ctx.backup_mut().set_eeprom_size(EepromSize::Size8K),
                _ => {}
            }
        }

        if log_enabled!(log::Level::Trace) {
            self.trace();
        }

        if self.src_addr_internal % self.word_len_internal != 0 {
            warn!(
                "DMA{} src addr misaligned: 0x{:08X}",
                self.ch, self.src_addr_internal
            );
        }
        if self.dest_addr_internal % self.word_len_internal != 0 {
            warn!(
                "DMA{} dest addr misaligned: 0x{:08X}",
                self.ch, self.dest_addr_internal
            );
        }
    }

    fn step(&mut self, ctx: &mut impl Context) {
        self.src_addr_internal = self.src_addr_internal.wrapping_add(self.src_inc);
        self.dest_addr_internal = self.dest_addr_internal.wrapping_add(self.dest_inc);

        self.word_count_internal -= 1;

        if self.word_count_internal == 0 {
            self.running = false;

            if !self.repeat() {
                self.enable = false;
            } else if self.dest_addr_ctrl == 3 {
                self.dest_addr_internal = self.dest_addr;
            }

            if self.irq_enable {
                let kind = match self.ch {
                    0 => InterruptKind::Dma0,
                    1 => InterruptKind::Dma1,
                    2 => InterruptKind::Dma2,
                    3 => InterruptKind::Dma3,
                    _ => unreachable!(),
                };
                ctx.interrupt_mut().set_interrupt(kind);
            }
        }
    }
}

impl Bus {
    pub fn process_dma(&mut self, ctx: &mut impl Context, ch: usize) -> bool {
        if !self.dma(ch).enable {
            return false;
        }

        if !self.dma(ch).running {
            if !self.dma_mut(ch).check_dma_start(ctx) {
                return false;
            }
            self.dma_mut(ch).start(ctx);
        }

        // The CPU is paused when DMA transfers are active, however, the CPU is operating during the periods when Sound/Blanking DMA transfers are paused.

        // Transfer Rate/Timing
        // Except for the first data unit, all units are transferred by sequential reads and writes. For n data units, the DMA transfer time is:

        //   2N+2(n-1)S+xI
        // Of which, 1N+(n-1)S are read cycles, and the other 1N+(n-1)S are write cycles, actual number of cycles depends on the waitstates and bus-width of the source and destination areas (as described in CPU Instruction Cycle Times chapter). Internal time for DMA processing is 2I (normally), or 4I (if both source and destination are in gamepak memory area).

        assert!(self.dma(ch).word_count_internal > 0);

        if self.dma(ch).word_len_internal == 4 {
            let data = self.read32(
                ctx,
                self.dma(ch).src_addr_internal & !3,
                self.dma(ch).first_access,
            );

            if let Some(data) = data {
                self.dma_buf = data;
            }

            self.write32(
                ctx,
                self.dma(ch).dest_addr_internal & !3,
                self.dma_buf,
                self.dma(ch).first_access,
            );
        } else {
            let data = self.read16(
                ctx,
                self.dma(ch).src_addr_internal & !1,
                self.dma(ch).first_access,
            );

            if let Some(data) = data {
                self.dma_buf = (data as u32) << 16 | data as u32;
            }

            self.write16(
                ctx,
                self.dma(ch).dest_addr_internal & !1,
                self.dma_buf as u16,
                self.dma(ch).first_access,
            );
        }

        self.dma_mut(ch).first_access = false;

        self.dma_mut(ch).step(ctx);

        true
    }
}
