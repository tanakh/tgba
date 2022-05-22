use std::{fmt::UpperHex, mem::size_of};

use bitvec::prelude::*;
use log::{info, log_enabled, trace, warn};

use crate::{
    context::{Interrupt, Lcd, Sound, SoundDma, Timing},
    interface::KeyInput,
    interrupt::InterruptKind,
    rom::{EepromSize, Rom},
    util::{pack, trait_alias},
};

trait_alias!(pub trait Context = Lcd + Sound + Timing + SoundDma + Interrupt);

pub struct Bus {
    bios: Vec<u8>,
    rom: Rom,
    ram: Vec<u8>,
    ext_ram: Vec<u8>,
    gamepak_ram: Vec<u8>,

    dma: [Dma; 4],
    timer: [Timer; 4],
    prev_cycle: u64,

    key_input: u16,
    key_interrupt_spec: u16,
    key_interrupt_enable: bool,
    key_interrupt_cond: bool, // 0: or, 1: and

    sio: Serial,

    game_pak_ram_wait_ctrl: u8,
    game_pak_wait_ctrl: [u8; 3],
    phi_terminal_output_ctrl: u8,
    prefetch_buffer: bool,
    game_pak_type: bool,

    post_boot: u8,

    wait_cycles: WaitCycles,

    reg_width_r: Vec<usize>,
    reg_width_w: Vec<usize>,
}

struct WaitCycles {
    gamepak_rom_1st_8: [u64; 3],
    gamepak_rom_1st_16: [u64; 3],
    gamepak_rom_1st_32: [u64; 3],
    gamepak_rom_2nd_8: [u64; 3],
    gamepak_rom_2nd_16: [u64; 3],
    gamepak_rom_2nd_32: [u64; 3],
    gamepak_ram_8: u64,
    gamepak_ram_16: u64,
    gamepak_ram_32: u64,
}

fn game_pak_ram_wait_cycle(ctrl: usize) -> u64 {
    const TBL: [u64; 4] = [4, 3, 2, 8];
    TBL[ctrl]
}

fn game_pak_rom_wait_cycle(ix: usize, ctrl: usize, first: bool) -> u64 {
    const FIRST: [u64; 8] = [4, 3, 2, 8, 4, 3, 2, 8];
    const SECOND: [[u64; 8]; 3] = [
        [2, 2, 2, 2, 1, 1, 1, 1],
        [4, 4, 4, 4, 1, 1, 1, 1],
        [8, 8, 8, 8, 1, 1, 1, 1],
    ];

    if first {
        FIRST[ctrl]
    } else {
        SECOND[ix][ctrl]
    }
}

impl WaitCycles {
    fn new(game_pak_wait_ctrl: &[u8; 3], game_pak_ram_wait_ctrl: u8) -> WaitCycles {
        let mut gamepak_rom_1st_8 = [0; 3];
        let mut gamepak_rom_1st_16 = [0; 3];
        let mut gamepak_rom_1st_32 = [0; 3];

        let mut gamepak_rom_2nd_8 = [0; 3];
        let mut gamepak_rom_2nd_16 = [0; 3];
        let mut gamepak_rom_2nd_32 = [0; 3];

        for ix in 0..3 {
            let ctrl = game_pak_wait_ctrl[ix] as usize;
            let wait_1st = game_pak_rom_wait_cycle(ix, ctrl, true);
            let wait_2nd = game_pak_rom_wait_cycle(ix, ctrl, false);
            gamepak_rom_1st_8[ix] = wait_1st;
            gamepak_rom_1st_16[ix] = wait_1st;
            gamepak_rom_1st_32[ix] = wait_1st + wait_2nd;
            gamepak_rom_2nd_8[ix] = wait_2nd;
            gamepak_rom_2nd_16[ix] = wait_2nd;
            gamepak_rom_2nd_32[ix] = wait_2nd + wait_2nd;
        }

        let ram_wait = game_pak_ram_wait_cycle(game_pak_ram_wait_ctrl as usize);
        let gamepak_ram_8 = ram_wait;
        let gamepak_ram_16 = ram_wait * 2;
        let gamepak_ram_32 = ram_wait * 3;

        WaitCycles {
            gamepak_rom_1st_8,
            gamepak_rom_1st_16,
            gamepak_rom_1st_32,
            gamepak_rom_2nd_8,
            gamepak_rom_2nd_16,
            gamepak_rom_2nd_32,
            gamepak_ram_8,
            gamepak_ram_16,
            gamepak_ram_32,
        }
    }
}

#[derive(Debug)]
struct Dma {
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
    fn new(ch: usize) -> Self {
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

    fn check_dma_start(&self, ctx: &mut impl Context) -> bool {
        match self.start_timing {
            // Start immetiately
            0 => true,
            // Start in a V-blank interval
            1 => self.prev_dma_frame != ctx.lcd().frame() && ctx.lcd().vblank(),
            // Start in an H-blank interval
            2 => {
                let lcd = ctx.lcd();
                lcd.hblank()
                    && (self.prev_dma_frame, self.prev_dma_line) != (lcd.frame(), lcd.line())
            }
            3 => match self.ch {
                // Prohibited
                0 => false,
                1 => ctx.sound_dma_request(0),
                2 => ctx.sound_dma_request(1),
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
        trace!("DMA{}:", self.ch);
        trace!("  - enable: {}", if self.dma_enable { "yes" } else { "no" });
        trace!(
            "  - src:    0x{:08X}, {}",
            self.src_addr,
            match self.src_addr_ctrl {
                0 => "inc",
                1 => "dec",
                2 => "fixed",
                3 => "inc/reload",
                _ => unreachable!(),
            }
        );
        trace!(
            "  - dest:   0x{:08X}, {}",
            self.dest_addr,
            match self.dest_addr_ctrl {
                0 => "inc",
                1 => "dec",
                2 => "fixed",
                3 => "prohibited",
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

#[derive(Default)]
struct Serial {
    // 00: 9600bps
    // 01: 19200bps
    // 10: 57600bps
    // 11: 115200bps
    baud_rate: u8,

    si_terminal: bool,
    sd_terminal: bool,

    // 00: Master
    // 01: 1st Slave
    // 10: 2nd Slave
    // 11: 3rd Slave
    multi_player_id: u8,

    communication_error: bool,

    // Master
    //   0: No transfer
    //   1: Start transfer
    // Slave
    //   0: Free
    //   1: Busy
    start_bit: bool,

    irq_enable: bool,

    data: [u16; 4],
    data8: u8,
}

impl Bus {
    pub fn new(bios: Vec<u8>, rom: Rom) -> Self {
        let ram = vec![0; 0x8000];
        let ext_ram = vec![0; 0x40000];
        let gamepak_ram = vec![0; 0x10000];

        let mut reg_width_r = vec![0; 0x800];
        let mut reg_width_w = vec![0; 0x800];

        for info in IO_REGS.iter() {
            let addr = info.addr & 0x3FF;
            let width = info.width;
            assert_eq!(addr & (width - 1) as u32, 0);

            for ofs in 0..width {
                if info.read {
                    reg_width_r[addr as usize + ofs] = width;
                }
                if info.write {
                    reg_width_w[addr as usize + ofs] = width;
                }
            }
        }

        let game_pak_ram_wait_ctrl = 0;
        let game_pak_wait_ctrl = [0; 3];

        let wait_cycles = WaitCycles::new(&game_pak_wait_ctrl, game_pak_ram_wait_ctrl);

        Bus {
            bios,
            rom,
            ram,
            ext_ram,
            gamepak_ram,

            dma: [0, 1, 2, 3].map(|ch| Dma::new(ch)),
            timer: Default::default(),
            prev_cycle: 0,

            key_input: 0x3FF,
            key_interrupt_spec: 0,
            key_interrupt_enable: false,
            key_interrupt_cond: false,

            sio: Default::default(),

            game_pak_ram_wait_ctrl,
            game_pak_wait_ctrl,

            phi_terminal_output_ctrl: 0,
            prefetch_buffer: false,
            game_pak_type: false,

            post_boot: 0,

            wait_cycles,

            reg_width_r,
            reg_width_w,
        }
    }

    pub fn set_key_input(&mut self, key_input: &KeyInput) {
        let v = self.key_input.view_bits_mut::<Lsb0>();
        v.set(0, !key_input.a);
        v.set(1, !key_input.b);
        v.set(2, !key_input.select);
        v.set(3, !key_input.start);
        v.set(4, !key_input.right);
        v.set(5, !key_input.left);
        v.set(6, !key_input.up);
        v.set(7, !key_input.down);
        v.set(8, !key_input.r);
        v.set(9, !key_input.l);

        // TODO: key interrupt
    }

    pub fn tick(&mut self, ctx: &mut impl Context) {
        for ch in 0..4 {
            self.process_dma(ctx, ch);
        }
        for ch in 0..2 {
            ctx.set_sound_dma_request(ch, false);
        }

        let prev_cycle = self.prev_cycle;
        let cur_cycle = ctx.now();
        let elapsed = cur_cycle - prev_cycle;
        self.prev_cycle = cur_cycle;

        let mut prev_overflow = 0;
        for ch in 0..4 {
            prev_overflow = self.timer[ch].process(ctx, ch, elapsed, prev_overflow);
        }
    }

    fn process_dma(&mut self, ctx: &mut impl Context, ch: usize) {
        if !self.dma[ch].dma_enable {
            return;
        }

        if !self.dma[ch].check_dma_start(ctx) {
            return;
        }

        // The CPU is paused when DMA transfers are active, however, the CPU is operating during the periods when Sound/Blanking DMA transfers are paused.

        // Transfer Rate/Timing
        // Except for the first data unit, all units are transferred by sequential reads and writes. For n data units, the DMA transfer time is:

        //   2N+2(n-1)S+xI
        // Of which, 1N+(n-1)S are read cycles, and the other 1N+(n-1)S are write cycles, actual number of cycles depends on the waitstates and bus-width of the source and destination areas (as described in CPU Instruction Cycle Times chapter). Internal time for DMA processing is 2I (normally), or 4I (if both source and destination are in gamepak memory area).

        self.dma[ch].prev_dma_frame = ctx.lcd().frame();
        self.dma[ch].prev_dma_line = ctx.lcd().line();

        let is_sound_dma = self.dma[ch].is_sound_dma();

        let word_len = if self.dma[ch].transfer_type || is_sound_dma {
            4
        } else {
            2
        };

        let word_count = if is_sound_dma {
            4
        } else {
            self.dma[ch].word_count
        };

        if ch == 3
            && self.is_valid_eeprom_addr(self.dma[ch].dest_addr_internal)
            && word_len == 2
            && matches!(self.rom.eeprom_size(), Some(None))
        {
            match word_count {
                9 => self.rom.set_eeprom_size(EepromSize::Size512),
                17 => self.rom.set_eeprom_size(EepromSize::Size8K),
                _ => warn!("Write EEPROM to unknown size with DMA: {}", word_count),
            }
        }

        let src_inc = match self.dma[ch].src_addr_ctrl {
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
            match self.dma[ch].dest_addr_ctrl {
                // increment
                0 => word_len,
                // decrement
                1 => -(word_len as i32) as u32,
                // fixed
                2 => 0,
                // increment / reload
                3 => 1,
                _ => unreachable!(),
            }
        };

        if log_enabled!(log::Level::Trace) {
            self.dma[ch].trace();
        }

        for i in 0..word_count {
            if word_len == 4 {
                let data = self.read32(ctx, self.dma[ch].src_addr_internal, i == 0);
                self.write32(ctx, self.dma[ch].dest_addr_internal, data, i == 0);
            } else {
                let data = self.read16(ctx, self.dma[ch].src_addr_internal, i == 0);
                self.write16(ctx, self.dma[ch].dest_addr_internal, data, i == 0);
            }

            self.dma[ch].src_addr_internal = self.dma[ch].src_addr_internal.wrapping_add(src_inc);
            self.dma[ch].dest_addr_internal =
                self.dma[ch].dest_addr_internal.wrapping_add(dest_inc);
        }

        if !self.dma[ch].repeat() {
            self.dma[ch].dma_enable = false;
        } else if self.dma[ch].dest_addr_ctrl == 3 {
            self.dma[ch].dest_addr_internal = self.dma[ch].dest_addr;
        }

        if self.dma[ch].irq_enable {
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
}

impl Bus {
    pub fn read8(&mut self, ctx: &mut impl Context, addr: u32, first: bool) -> u8 {
        // trace!("Read8: 0x{addr:08X}");

        match addr >> 24 {
            0x0..=0x1 => {
                ctx.elapse(1);
                if addr < 0x00004000 {
                    self.bios[addr as usize]
                } else {
                    warn!("Invalid BIOS address read: 0x{addr:08X}:8");
                    0
                }
            }
            0x2 => {
                ctx.elapse(3);
                self.ext_ram[(addr & 0x3FFFF) as usize]
            }
            0x3 => {
                ctx.elapse(1);
                self.ram[(addr & 0x7FFF) as usize]
            }

            0x4 => {
                ctx.elapse(1);
                let data = self.io_read8(ctx, addr & 0xFFFF);
                trace_io::<u8, true>(addr, data);
                data
            }

            0x5 => {
                ctx.elapse(1);
                ctx.lcd().palette[(addr & 0x3FF) as usize]
            }
            0x6 => {
                ctx.elapse(1);
                ctx.lcd().vram[vram_addr(addr)]
            }
            0x7 => {
                ctx.elapse(1);
                ctx.lcd().oam[(addr & 0x3FF) as usize]
            }

            0x8..=0xD => {
                let ix = (addr >> 25) as usize - 4;
                ctx.elapse(if first {
                    self.wait_cycles.gamepak_rom_1st_8[ix]
                } else {
                    self.wait_cycles.gamepak_rom_2nd_8[ix]
                });

                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    self.rom.data[ofs]
                } else {
                    warn!("Read from invalid Game Pak ROM address: {addr:08X}");
                    0
                }
            }

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_8);
                self.gamepak_ram[(addr & 0xFFFF) as usize]
            }
            _ => panic!(),
        }
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32, first: bool) -> u16 {
        // trace!("Read16: 0x{addr:08X}");

        match addr >> 24 {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    ctx.elapse(1);
                    read16(&self.bios, addr as usize)
                } else {
                    warn!("Invalid BIOS address read: 0x{addr:08X}:16");
                    0
                }
            }
            0x2 => {
                ctx.elapse(3);
                read16(&self.ext_ram, (addr & 0x3FFFF) as usize)
            }
            0x3 => {
                ctx.elapse(1);
                read16(&self.ram, (addr & 0x7FFF) as usize)
            }

            0x4 => {
                ctx.elapse(1);
                let data = self.io_read16(ctx, addr & 0xFFFF);
                trace_io::<u16, true>(addr, data);
                data
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(1);
                read16(&ctx.lcd().palette, (addr & 0x3FF) as usize)
            }
            0x6 => {
                ctx.elapse(1);
                read16(&ctx.lcd().vram, vram_addr(addr))
            }
            0x7 => {
                ctx.elapse(1);
                read16(&ctx.lcd().oam, (addr & 0x3FF) as usize)
            }

            0x8..=0xD => {
                let ix = (addr >> 25) as usize - 4;
                ctx.elapse(if first {
                    self.wait_cycles.gamepak_rom_1st_16[ix]
                } else {
                    self.wait_cycles.gamepak_rom_2nd_16[ix]
                });

                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read16(&self.rom.data, ofs)
                } else if self.is_valid_eeprom_addr(addr) {
                    self.rom.read_eeprom() as u16
                } else {
                    warn!("Write to invalid Game Pak ROM address: {addr:08X}");
                    0
                }
            }

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_16);
                read16(&self.gamepak_ram, (addr & 0xFFFF) as usize)
            }
            _ => panic!(),
        }
    }

    pub fn read32(&mut self, ctx: &mut impl Context, addr: u32, first: bool) -> u32 {
        // trace!("Read32: 0x{addr:08X}");

        match addr >> 24 {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    ctx.elapse(1);
                    read32(&self.bios, addr as usize)
                } else {
                    warn!("Invalid BIOS address read: 0x{addr:08X}:32");
                    0
                }
            }
            0x2 => {
                ctx.elapse(6);
                read32(&self.ext_ram, (addr & 0x3FFFF) as usize)
            }
            0x3 => {
                ctx.elapse(1);
                read32(&self.ram, (addr & 0x7FFF) as usize)
            }

            0x4 => {
                ctx.elapse(1);
                let addr = addr & 0xFFFF;
                let lo = self.io_read16(ctx, addr);
                let hi = self.io_read16(ctx, addr + 2);
                let data = (hi as u32) << 16 | lo as u32;
                trace_io::<u32, true>(addr, data);
                data
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(2);
                read32(&ctx.lcd().palette, (addr & 0x3FF) as usize)
            }
            0x6 => {
                ctx.elapse(2);
                read32(&ctx.lcd().vram, vram_addr(addr))
            }
            0x7 => {
                ctx.elapse(1);
                read32(&ctx.lcd().oam, (addr & 0x3FF) as usize)
            }

            0x8..=0xD => {
                let ix = (addr >> 25) as usize - 4;
                ctx.elapse(if first {
                    self.wait_cycles.gamepak_rom_1st_32[ix]
                } else {
                    self.wait_cycles.gamepak_rom_2nd_32[ix]
                });

                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read32(&self.rom.data, ofs)
                } else {
                    warn!("Read from invalid Game Pak ROM address: {addr:08X}");
                    0
                }
            }

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_32);
                read32(&self.gamepak_ram, (addr & 0xFFFF) as usize)
            }
            _ => panic!("Read: 0x{addr:08X}"),
        }
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8, first: bool) {
        // trace!("Write8: 0x{addr:08X} = 0x{data:02X}");

        match addr >> 24 {
            0x0..=0x1 => {
                // FIXME: ???
                warn!("Write to BIOS");
            }
            0x2 => {
                ctx.elapse(3);
                self.ext_ram[(addr & 0x3FFFF) as usize] = data;
            }
            0x3 => {
                ctx.elapse(1);
                self.ram[(addr & 0x7FFF) as usize] = data
            }

            0x4 => {
                ctx.elapse(1);
                trace_io::<u8, false>(addr, data);
                self.io_write8(ctx, addr & 0xFFFF, data);
            }

            0x5 => {
                warn!("Write 8bit data to Palette: 0x{addr:08X} = 0x{data:02X}");
                ctx.elapse(1);
                ctx.lcd_mut().palette[(addr & 0x3FF) as usize] = data;
            }
            0x6 => {
                warn!("Write 8bit data to VRAM: 0x{addr:08X} = 0x{data:02X}");
                ctx.elapse(1);
                ctx.lcd_mut().vram[vram_addr(addr)] = data;
            }
            0x7 => {
                warn!("Write 8bit data to OAM: 0x{addr:08X} = 0x{data:02X}");
                ctx.elapse(1);
                ctx.lcd_mut().oam[(addr & 0x3FF) as usize] = data;
            }

            0x8..=0xD => warn!("Write 8bit data to ROM"),

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_8);
                self.gamepak_ram[(addr & 0xFFFF) as usize] = data;
            }
            _ => panic!(),
        }
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16, first: bool) {
        // trace!("Write16: 0x{addr:08X} = 0x{data:04X}");

        match addr >> 24 {
            0x0..=0x1 => {
                warn!("Write to BIOS");
            }
            0x2 => {
                ctx.elapse(3);
                write16(&mut self.ext_ram, (addr & 0x3FFFF) as usize, data);
            }
            0x3 => {
                ctx.elapse(1);
                write16(&mut self.ram, (addr & 0x7FFF) as usize, data);
            }

            0x4 => {
                ctx.elapse(1);
                trace_io::<u16, false>(addr, data);
                self.io_write16(ctx, addr & 0xFFFF, data);
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(1);
                write16(&mut ctx.lcd_mut().palette, (addr & 0x3FF) as usize, data);
            }
            0x6 => {
                ctx.elapse(1);
                write16(&mut ctx.lcd_mut().vram, vram_addr(addr), data);
            }
            0x7 => {
                ctx.elapse(1);
                write16(&mut ctx.lcd_mut().oam, (addr & 0x3FF) as usize, data);
            }

            0x8..=0xD => {
                let ix = (addr >> 25) as usize - 4;
                ctx.elapse(if first {
                    self.wait_cycles.gamepak_rom_1st_16[ix]
                } else {
                    self.wait_cycles.gamepak_rom_2nd_16[ix]
                });

                if self.is_valid_eeprom_addr(addr) {
                    self.rom.write_eeprom(data & 1 != 0);
                } else {
                    warn!("Write to invalid Game Pak ROM address: {addr:08X}");
                }
            }

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_16);
                write16(&mut self.gamepak_ram, (addr & 0xFFFF) as usize, data);
            }
            _ => panic!(),
        }
    }

    pub fn write32(&mut self, ctx: &mut impl Context, addr: u32, data: u32, first: bool) {
        // trace!("Write32: 0x{addr:08X} = 0x{data:08X}");

        match addr >> 24 {
            0x0..=0x1 => {
                warn!("Write to BIOS");
            }
            0x2 => {
                ctx.elapse(6);
                write32(&mut self.ext_ram, (addr & 0x3FFFF) as usize, data);
            }
            0x3 => {
                ctx.elapse(1);
                write32(&mut self.ram, (addr & 0x7FFF) as usize, data);
            }

            0x4 => {
                ctx.elapse(1);
                trace_io::<u32, false>(addr, data);
                let addr = addr & 0xFFFF;
                self.io_write16(ctx, addr, data as u16);
                self.io_write16(ctx, addr + 2, (data >> 16) as u16);
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(2);
                write32(&mut ctx.lcd_mut().palette, (addr & 0x3FF) as usize, data);
            }
            0x6 => {
                ctx.elapse(2);
                write32(&mut ctx.lcd_mut().vram, vram_addr(addr), data);
            }
            0x7 => {
                ctx.elapse(1);
                write32(&mut ctx.lcd_mut().oam, (addr & 0x3FF) as usize, data);
            }

            0x8..=0xD => warn!("Write 32bit data to ROM"),

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_32);
                write32(&mut self.gamepak_ram, (addr & 0xFFFF) as usize, data);
            }
            _ => panic!("Write32: 0x{addr:08X} = 0x{data:08X}"),
        }
    }
}

fn vram_addr(addr: u32) -> usize {
    (if addr & 0x10000 == 0 {
        addr & 0xFFFF
    } else {
        addr & 0x17FFF
    }) as usize
}

impl Bus {
    pub fn io_read8(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        match addr {
            0x000..=0x05F => {
                let data = ctx.lcd_read16(addr & !1);
                (data >> ((addr & 1) * 8)) as u8
            }
            0x060..=0x0AF => ctx.sound_read8(addr),

            // KEYINPUT
            0x130 => self.key_input as u8,
            0x131 => (self.key_input >> 8) as u8,

            // IME
            0x208 => ctx.interrupt_mut().master_enable() as u8,
            0x209 | 0x20A | 0x20B => 0,

            // POSTFLG
            0x300 => self.post_boot,

            0xF600..=0xFFFF => 0,

            _ => todo!(
                "IO read8: 0x{addr:03X} ({})",
                get_io_reg(addr).map_or("N/A", |r| r.name)
            ),
        }
    }

    pub fn io_read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        match addr {
            0x000..=0x05E => ctx.lcd_read16(addr),
            0x060..=0x0AE => ctx.sound_read16(addr),

            // DMAxSAD
            0x0B0 | 0x0BC | 0x0C8 | 0x0D4 => 0,
            0x0B2 | 0x0BE | 0x0CA | 0x0D6 => 0,

            // DMAxDAD
            0x0B4 | 0x0C0 | 0x0CC | 0x0D8 => 0,
            0x0B6 | 0x0C2 | 0x0CE | 0x0DA => 0,

            // DMAxCNT
            0x0B8 | 0x0C4 | 0x0D0 | 0x0DC => 0,
            0x0BA | 0x0C6 | 0x0D2 | 0x0DE => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                let dma = &mut self.dma[i];
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

            0x0E0..=0x0FE => 0,

            // TMxCNT_L
            0x100 | 0x104 | 0x108 | 0x10C => {
                let i = ((addr - 0x100) / 0x4) as usize;
                self.timer[i].counter
            }
            // TMxCNT_H
            0x102 | 0x106 | 0x10A | 0x10E => {
                let i = ((addr - 0x100) / 0x4) as usize;
                let timer = &mut self.timer[i];
                pack! {
                    0..=1   => timer.prescaler,
                    2 => timer.countup_timing,
                    6 => timer.irq_enable,
                    7 => timer.enable,
                }
            }

            // SIOCNT
            0x128 => {
                // TODO
                warn!("Read SIOCNT");
                0
            }

            // KEYCNT
            0x132 => pack! {
                0..=9 => self.key_interrupt_spec,
                14    => self.key_interrupt_enable,
                15    => self.key_interrupt_cond,
            },

            // JOY_RECV
            0x150 => 0,
            0x152 => 0,

            // IE
            0x200 => ctx.interrupt().enable(),
            // IF
            0x202 => ctx.interrupt().request(),

            // WAITCNT
            0x204 => pack! {
                0..=1   => self.game_pak_ram_wait_ctrl,
                2..=4   => self.game_pak_wait_ctrl[0],
                5..=7   => self.game_pak_wait_ctrl[1],
                8..=10  => self.game_pak_wait_ctrl[2],
                11..=12 => self.phi_terminal_output_ctrl,
                14      => self.prefetch_buffer,
                15      => self.game_pak_type,
            },

            0x130 | 0x208 | 0x20A => {
                let lo = self.io_read8(ctx, addr);
                let hi = self.io_read8(ctx, addr + 1);
                lo as u16 | ((hi as u16) << 8)
            }

            0x100C => 0,

            0xF600..=0xFFFE => 0,

            _ => todo!(
                "IO read16: 0x{addr:03X} ({})",
                get_io_reg(addr).map_or("N/A", |r| r.name)
            ),
        }
    }

    pub fn io_write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        match addr {
            0x000..=0x05F => {
                warn!(
                    "Writing 8bit data to LCD registers are ignored: 0x{addr:03X} = 0x{data:02X}"
                );
            }
            0x060..=0x0AF => ctx.sound_write8(addr, data),

            // SIODATA
            0x120..=0x127 => {
                let i = (addr - 0x120) as usize;
                warn!("SIODATA[{i}] = 0x{data:02X}");
            }

            // SIOCNT
            0x128 | 0x129 => {
                let i = (addr - 0x128) as usize;
                warn!("SIOCNT[{i}] = 0x{data:02X}");
            }

            // SIODATA8
            0x12A => self.sio.data8 = data as u8,
            0x12B..=0x12F => {}

            // JOYCNT
            0x140 => {
                info!("JOYCNT = 0x{data:02X}");
            }
            0x141..=0x14F => {}

            // IF
            0x202 => ctx.interrupt_mut().reset_request(data as u16),
            0x203 => ctx.interrupt_mut().reset_request((data as u16 & 0x3F) << 8),

            // IME
            0x208 => ctx.interrupt_mut().set_master_enable((data & 1) != 0),

            // POSTFLG
            0x300 => self.post_boot = data,

            // HALTCNT
            0x301 => {
                if data == 0x00 {
                    info!("Enter halt mode");
                    ctx.interrupt_mut().set_halt(true);
                } else if data == 0x80 {
                    // FIXME
                    panic!("Enter stop mode");
                    // ctx.interrupt_mut().set_stop(true);
                }
            }

            // ???
            0x410 => {}

            0x209 => {}

            0xF600..=0xFFFF => {
                let data = data as char;
                if data == '\0' {
                    println!();
                } else if data.is_ascii_graphic() || data.is_ascii_whitespace() {
                    print!("{data}");
                }
            }

            _ => todo!(
                "IO write8: 0x{addr:03X} ({}) = 0x{data:02X}",
                get_io_reg(addr).map_or("N/A", |r| r.name)
            ),
        }
    }

    pub fn io_write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        match addr {
            0x000..=0x05E => ctx.lcd_write16(addr, data),
            0x060..=0x0AE => ctx.sound_write16(addr, data),

            // DMAxSAD
            0x0B0 | 0x0BC | 0x0C8 | 0x0D4 => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                self.dma[i].src_addr.view_bits_mut::<Lsb0>()[0..=15].store(data);
            }
            0x0B2 | 0x0BE | 0x0CA | 0x0D6 => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                let hi = if i == 0 { 26 } else { 27 };
                self.dma[i].src_addr.view_bits_mut::<Lsb0>()[16..=hi].store(data);
            }

            // DMAxDAD
            0x0B4 | 0x0C0 | 0x0CC | 0x0D8 => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                self.dma[i].dest_addr.view_bits_mut::<Lsb0>()[0..=15].store(data)
            }
            0x0B6 | 0x0C2 | 0x0CE | 0x0DA => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                let hi = if i != 3 { 26 } else { 27 };
                self.dma[i].dest_addr.view_bits_mut::<Lsb0>()[16..=hi].store(data)
            }

            // DMAxCNT
            0x0B8 | 0x0C4 | 0x0D0 | 0x0DC => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                let mask = if i != 3 { 0x3FFF } else { 0xFFFF };
                let data = (data & mask) as u32;
                self.dma[i].word_count = if data == 0 { mask as u32 + 1 } else { data };
            }
            0x0BA | 0x0C6 | 0x0D2 | 0x0DE => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                let dma = &mut self.dma[i];

                let v = data.view_bits::<Lsb0>();
                dma.dest_addr_ctrl = v[5..=6].load();
                dma.src_addr_ctrl = v[7..=8].load();
                dma.repeat = v[9];
                dma.transfer_type = v[10];
                if i == 3 {
                    dma.game_pak_data_request_transfer = v[11];
                }
                dma.start_timing = v[12..=13].load();
                dma.irq_enable = v[14];

                if !dma.dma_enable && v[15] {
                    // Reload src addr, dest addr and word count
                    dma.src_addr_internal = dma.src_addr;
                    dma.dest_addr_internal = dma.dest_addr;
                    dma.word_count_internal = dma.word_count;
                }

                dma.dma_enable = v[15];
            }

            0x0E0..=0x0FE => {}

            // TMxCNT_L
            0x100 | 0x104 | 0x108 | 0x10C => {
                let i = ((addr - 0x100) / 0x4) as usize;
                self.timer[i].reload = data;
            }
            // TMxCNT_H
            0x102 | 0x106 | 0x10A | 0x10E => {
                let data = data.view_bits::<Lsb0>();
                let i = ((addr - 0x100) / 0x4) as usize;
                let timer = &mut self.timer[i];
                timer.prescaler = data[0..=1].load();
                timer.countup_timing = data[2];
                timer.irq_enable = data[6];
                if !timer.enable && data[7] {
                    timer.counter = timer.reload;
                    timer.fraction = 0;
                }
                timer.enable = data[7];
            }

            0x110..=0x11E => {}

            // KEYINPUT
            0x130 => {} // ???
            // KEYCNT
            0x132 => {
                let data = data.view_bits::<Lsb0>();
                self.key_interrupt_spec = data[0..=9].load();
                self.key_interrupt_enable = data[14];
                self.key_interrupt_cond = data[15];
            }

            // RCNT
            0x134 => {
                info!("RCNT = 0x{data:04X}");
            }

            // JOY_RECV_L
            0x150 => {
                info!("JOY_RECV_L = 0x{data:04X}");
            }
            // JOY_RECV_H
            0x152 => {
                info!("JOY_RECV_H = 0x{data:04X}");
            }
            // JOY_TRANS_L
            0x154 => {
                info!("JOY_TRANS_L = 0x{data:04X}");
            }
            // JOY_TRANS_H
            0x156 => {
                info!("JOY_TRANS_H = 0x{data:04X}");
            }
            // JOYSTAT
            0x158 => {
                info!("JOYSTAT = 0x{data:04X}");
            }
            0x15A..=0x15E => {}

            0x200 => ctx.interrupt_mut().set_enable(data & 0x3FFF),
            0x202 => {
                self.io_write8(ctx, addr, data as u8);
                self.io_write8(ctx, addr + 1, (data >> 8) as u8);
            }

            // Game Pak Memory Wait Control
            0x204 => {
                let v = data.view_bits::<Lsb0>();
                self.game_pak_ram_wait_ctrl = v[0..=1].load();
                self.game_pak_wait_ctrl[0] = v[2..=4].load();
                self.game_pak_wait_ctrl[1] = v[5..=7].load();
                self.game_pak_wait_ctrl[2] = v[8..=10].load();
                self.phi_terminal_output_ctrl = v[11..=12].load();
                self.prefetch_buffer = v[14];
                self.game_pak_type = v[15];

                self.wait_cycles =
                    WaitCycles::new(&self.game_pak_wait_ctrl, self.game_pak_ram_wait_ctrl);
            }

            0x206 | 0x20A | 0x20C..=0x21E | 0x100C => {}

            _ => {
                self.io_write8(ctx, addr, data as u8);
                self.io_write8(ctx, addr + 1, (data >> 8) as u8);
            }
        }
    }

    fn is_valid_eeprom_addr(&self, addr: u32) -> bool {
        if addr & 0x08000000 == 0 {
            return false;
        }
        let ofs = addr & 0x01FFFFFF;
        let large_rom = self.rom.data.len() >= 0x01000000;
        (!large_rom && ofs & 0x01000000 != 0) || (large_rom && ofs & 0x01FFFF00 == 0x01FFFF00)
    }
}

fn read16(p: &[u8], addr: usize) -> u16 {
    u16::from_le_bytes(p[addr..addr + 2].try_into().unwrap())
}

fn read32(p: &[u8], addr: usize) -> u32 {
    u32::from_le_bytes(p[addr..addr + 4].try_into().unwrap())
}

fn write16(p: &mut [u8], addr: usize, data: u16) {
    p[addr..addr + 2].copy_from_slice(&data.to_le_bytes());
}

fn write32(p: &mut [u8], addr: usize, data: u32) {
    p[addr..addr + 4].copy_from_slice(&data.to_le_bytes());
}

fn trace_io<T: UpperHex, const READ: bool>(addr: u32, data: T) {
    let addr = addr & 0xFFFF;

    if !log::log_enabled!(log::Level::Trace) {
        return;
    }

    let size = size_of::<T>() * 8;

    let data = if size == 8 {
        format!("{data:02X}")
    } else if size == 16 {
        format!("{data:04X}")
    } else {
        format!("{data:08X}")
    };

    let annot = if let Some(reg) = get_io_reg(addr) {
        format!("{} - {}", reg.name, reg.description)
    } else {
        "N/A".to_string()
    };

    let dir = if READ { "Read" } else { "Write" };

    trace!("{dir}{size}: 0x{addr:03X} = 0x{data} # {annot}");
}

fn get_io_reg(addr: u32) -> Option<&'static IoReg> {
    IO_REGS.iter().find(|r| r.addr & 0xFFFF == addr)
}

struct IoReg {
    addr: u32,
    width: usize,
    read: bool,
    write: bool,
    name: &'static str,
    description: &'static str,
}

macro_rules! io_regs {
    ($addr:literal $width:literal R/W $name:ident $desc:literal $($rest:tt)*) => {
        io_regs!($($rest)* [$addr, $width, true, true, $name, $desc])
    };
    ($addr:literal $width:literal R $name:ident $desc:literal $($rest:tt)*) => {
        io_regs!($($rest)* [$addr, $width, true, false, $name, $desc])
    };
    ($addr:literal $width:literal W $name:ident $desc:literal $($rest:tt)*) => {
        io_regs!($($rest)* [$addr, $width, false, true, $name, $desc])
    };

    (@end $($entry:tt)*) => {
        &[ $(io_regs!(@entry $entry)),* ]
    };

    (@entry [$addr:literal, $witdh:literal, $r:literal, $w:literal, $name:ident, $desc:literal]) => {
        IoReg {
            addr: $addr,
            width: $witdh,
            read: $r,
            write: $w,
            name: stringify!($name),
            description: $desc,
        }
    };
}

const IO_REGS: &[IoReg] = io_regs! {
    // LCD
    0x4000000  2    R/W  DISPCNT   "LCD Control"
    0x4000002  2    R/W  NA         "Undocumented - Green Swap"
    0x4000004  2    R/W  DISPSTAT  "General LCD Status (STAT,LYC)"
    0x4000006  2    R    VCOUNT    "Vertical Counter (LY)"
    0x4000008  2    R/W  BG0CNT    "BG0 Control"
    0x400000A  2    R/W  BG1CNT    "BG1 Control"
    0x400000C  2    R/W  BG2CNT    "BG2 Control"
    0x400000E  2    R/W  BG3CNT    "BG3 Control"
    0x4000010  2    W    BG0HOFS   "BG0 X-Offset"
    0x4000012  2    W    BG0VOFS   "BG0 Y-Offset"
    0x4000014  2    W    BG1HOFS   "BG1 X-Offset"
    0x4000016  2    W    BG1VOFS   "BG1 Y-Offset"
    0x4000018  2    W    BG2HOFS   "BG2 X-Offset"
    0x400001A  2    W    BG2VOFS   "BG2 Y-Offset"
    0x400001C  2    W    BG3HOFS   "BG3 X-Offset"
    0x400001E  2    W    BG3VOFS   "BG3 Y-Offset"
    0x4000020  2    W    BG2PA     "BG2 Rotation/Scaling Parameter A (dx)"
    0x4000022  2    W    BG2PB     "BG2 Rotation/Scaling Parameter B (dmx)"
    0x4000024  2    W    BG2PC     "BG2 Rotation/Scaling Parameter C (dy)"
    0x4000026  2    W    BG2PD     "BG2 Rotation/Scaling Parameter D (dmy)"
    0x4000028  4    W    BG2X      "BG2 Reference Point X-Coordinate"
    0x400002C  4    W    BG2Y      "BG2 Reference Point Y-Coordinate"
    0x4000030  2    W    BG3PA     "BG3 Rotation/Scaling Parameter A (dx)"
    0x4000032  2    W    BG3PB     "BG3 Rotation/Scaling Parameter B (dmx)"
    0x4000034  2    W    BG3PC     "BG3 Rotation/Scaling Parameter C (dy)"
    0x4000036  2    W    BG3PD     "BG3 Rotation/Scaling Parameter D (dmy)"
    0x4000038  4    W    BG3X      "BG3 Reference Point X-Coordinate"
    0x400003C  4    W    BG3Y      "BG3 Reference Point Y-Coordinate"
    0x4000040  2    W    WIN0H     "Window 0 Horizontal Dimensions"
    0x4000042  2    W    WIN1H     "Window 1 Horizontal Dimensions"
    0x4000044  2    W    WIN0V     "Window 0 Vertical Dimensions"
    0x4000046  2    W    WIN1V     "Window 1 Vertical Dimensions"
    0x4000048  2    R/W  WININ     "Inside of Window 0 and 1"
    0x400004A  2    R/W  WINOUT    "Inside of OBJ Window & Outside of Windows"
    0x400004C  2    W    MOSAIC    "Mosaic Size"
    0x4000050  2    R/W  BLDCNT    "Color Special Effects Selection"
    0x4000052  2    R/W  BLDALPHA  "Alpha Blending Coefficients"
    0x4000054  2    W    BLDY      "Brightness (Fade-In/Out) Coefficient"

    // Sound
    0x4000060  2  R/W  SOUND1CNT_L "Channel 1 Sweep register       (NR10)"
    0x4000062  2  R/W  SOUND1CNT_H "Channel 1 Duty/Length/Envelope (NR11, NR12)"
    0x4000064  2  R/W  SOUND1CNT_X "Channel 1 Frequency/Control    (NR13, NR14)"
    0x4000068  2  R/W  SOUND2CNT_L "Channel 2 Duty/Length/Envelope (NR21, NR22)"
    0x400006C  2  R/W  SOUND2CNT_H "Channel 2 Frequency/Control    (NR23, NR24)"
    0x4000070  2  R/W  SOUND3CNT_L "Channel 3 Stop/Wave RAM select (NR30)"
    0x4000072  2  R/W  SOUND3CNT_H "Channel 3 Length/Volume        (NR31, NR32)"
    0x4000074  2  R/W  SOUND3CNT_X "Channel 3 Frequency/Control    (NR33, NR34)"
    0x4000078  2  R/W  SOUND4CNT_L "Channel 4 Length/Envelope      (NR41, NR42)"
    0x400007C  2  R/W  SOUND4CNT_H "Channel 4 Frequency/Control    (NR43, NR44)"
    0x4000080  2  R/W  SOUNDCNT_L  "Control Stereo/Volume/Enable   (NR50, NR51)"
    0x4000082  2  R/W  SOUNDCNT_H  "Control Mixing/DMA Control"
    0x4000084  2  R/W  SOUNDCNT_X  "Control Sound on/off           (NR52)"
    0x4000088  2  R/W  SOUNDBIAS   "Sound PWM Control"
    // 0x4000090 2x10h R/W  WAVE_RAM  "Channel 3 Wave Pattern RAM (2 banks!!)"
    0x40000A0  4    W    FIFO_A    "Channel A FIFO, Data 0-3"
    0x40000A4  4    W    FIFO_B    "Channel B FIFO, Data 0-3"

    // DMA
    0x40000B0  4    W    DMA0SAD   "DMA 0 Source Address"
    0x40000B4  4    W    DMA0DAD   "DMA 0 Destination Address"
    0x40000B8  2    W    DMA0CNT_L "DMA 0 Word Count"
    0x40000BA  2    R/W  DMA0CNT_H "DMA 0 Control"
    0x40000BC  4    W    DMA1SAD   "DMA 1 Source Address"
    0x40000C0  4    W    DMA1DAD   "DMA 1 Destination Address"
    0x40000C4  2    W    DMA1CNT_L "DMA 1 Word Count"
    0x40000C6  2    R/W  DMA1CNT_H "DMA 1 Control"
    0x40000C8  4    W    DMA2SAD   "DMA 2 Source Address"
    0x40000CC  4    W    DMA2DAD   "DMA 2 Destination Address"
    0x40000D0  2    W    DMA2CNT_L "DMA 2 Word Count"
    0x40000D2  2    R/W  DMA2CNT_H "DMA 2 Control"
    0x40000D4  4    W    DMA3SAD   "DMA 3 Source Address"
    0x40000D8  4    W    DMA3DAD   "DMA 3 Destination Address"
    0x40000DC  2    W    DMA3CNT_L "DMA 3 Word Count"
    0x40000DE  2    R/W  DMA3CNT_H "DMA 3 Control"

    // Timer
    0x4000100  2    R/W  TM0CNT_L  "Timer 0 Counter/Reload"
    0x4000102  2    R/W  TM0CNT_H  "Timer 0 Control"
    0x4000104  2    R/W  TM1CNT_L  "Timer 1 Counter/Reload"
    0x4000106  2    R/W  TM1CNT_H  "Timer 1 Control"
    0x4000108  2    R/W  TM2CNT_L  "Timer 2 Counter/Reload"
    0x400010A  2    R/W  TM2CNT_H  "Timer 2 Control"
    0x400010C  2    R/W  TM3CNT_L  "Timer 3 Counter/Reload"
    0x400010E  2    R/W  TM3CNT_H  "Timer 3 Control"

    // Serial
    0x4000120  4    R/W  SIODATA32 "SIO Data (Normal-32bit Mode; shared with below)"
    // 0x4000120  2    R/W  SIOMULTI0 "SIO Data 0 (Parent)    (Multi-Player Mode)"
    0x4000122  2    R/W  SIOMULTI1 "SIO Data 1 (1st Child) (Multi-Player Mode)"
    0x4000124  2    R/W  SIOMULTI2 "SIO Data 2 (2nd Child) (Multi-Player Mode)"
    0x4000126  2    R/W  SIOMULTI3 "SIO Data 3 (3rd Child) (Multi-Player Mode)"
    0x4000128  2    R/W  SIOCNT    "SIO Control Register"
    // 0x400012A  2    R/W  SIOMLT_SEND "SIO Data (Local of MultiPlayer; shared below)"
    0x400012A  2    R/W  SIODATA8  "SIO Data (Normal-8bit and UART Mode)"

    // Keypad
    0x4000130  2    R    KEYINPUT  "Key Status"
    0x4000132  2    R/W  KEYCNT    "Key Interrupt Control"

    // Serial
    0x4000134  2    R/W  RCNT      "SIO Mode Select/General Purpose Data"
    // 0x4000136  -    -    IR        "Ancient - Infrared Register (Prototypes only)"
    0x4000140  2    R/W  JOYCNT    "SIO JOY Bus Control"
    0x4000150  4    R/W  JOY_RECV  "SIO JOY Bus Receive Data"
    0x4000154  4    R/W  JOY_TRANS "SIO JOY Bus Transmit Data"
    // 0x4000158  2    R/?  JOYSTAT   "SIO JOY Bus Receive Status"
    0x4000158  2    R/W  JOYSTAT   "SIO JOY Bus Receive Status"

    // Interrupt, Waitstate, and Power-Down Control
    0x4000200  2    R/W  IE        "Interrupt Enable Register"
    0x4000202  2    R/W  IF        "Interrupt Request Flags / IRQ Acknowledge"
    0x4000204  2    R/W  WAITCNT   "Game Pak Waitstate Control"
    0x4000208  2    R/W  IME       "Interrupt Master Enable Register"
    0x4000300  1    R/W  POSTFLG   "Undocumented - Post Boot Flag"
    0x4000301  1    W    HALTCNT   "Undocumented - Power Down Control"
    // 0x4000410  ?    ?    ?         "Undocumented - Purpose Unknown / Bug ??? 0FFh"
    // 0x4000800  4    R/W  ?         "Undocumented - Internal Memory Control (R/W)"
    // 0x4xx0800  4    R/W  ?         "Mirrors of 4000800h (repeated each 64K)"

    @end
};
