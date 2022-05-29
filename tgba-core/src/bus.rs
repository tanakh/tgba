use std::{cmp::min, fmt::UpperHex, mem::size_of};

use bitvec::prelude::*;
use log::{debug, info, trace, warn};

use crate::{
    backup::Backup,
    context::{Interrupt, Lcd, Sound, SoundDma, Timing},
    dma::Dma,
    interface::KeyInput,
    interrupt::InterruptKind,
    ioreg_info::get_io_reg,
    rom::Rom,
    serial::Serial,
    timer::Timers,
    util::{pack, read16, read32, trait_alias, write16, write32},
};

trait_alias!(pub trait Context = Lcd + Sound + Timing + SoundDma + Interrupt);

pub struct Bus {
    bios: Vec<u8>,
    pub ram: Vec<u8>, // FIXME: remove pub
    ext_ram: Vec<u8>,
    rom: Rom,
    backup: Backup,

    dma: [Dma; 4],
    timers: Timers,

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

    prefetch_base: u32,
    prefetched_size: u32,
    prefetch_start_time: u64,

    wait_cycles: WaitCycles,
}

#[derive(Debug)]
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
            gamepak_rom_1st_8[ix] = wait_1st + 1;
            gamepak_rom_1st_16[ix] = wait_1st + 1;
            gamepak_rom_1st_32[ix] = wait_1st + wait_2nd + 2;
            gamepak_rom_2nd_8[ix] = wait_2nd + 1;
            gamepak_rom_2nd_16[ix] = wait_2nd + 1;
            gamepak_rom_2nd_32[ix] = wait_2nd + wait_2nd + 2;
        }

        let ram_wait = game_pak_ram_wait_cycle(game_pak_ram_wait_ctrl as usize);
        let gamepak_ram_8 = ram_wait + 1;
        let gamepak_ram_16 = ram_wait * 2 + 2;
        let gamepak_ram_32 = ram_wait * 3 + 3;

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

impl Bus {
    pub fn new(bios: Vec<u8>, rom: Rom) -> Self {
        let ram = vec![0; 0x8000];
        let ext_ram = vec![0; 0x40000];
        let backup = Backup::detect_backup(&rom.data);

        let game_pak_ram_wait_ctrl = 0;
        let game_pak_wait_ctrl = [0; 3];

        let wait_cycles = WaitCycles::new(&game_pak_wait_ctrl, game_pak_ram_wait_ctrl);

        Bus {
            bios,
            ram,
            ext_ram,
            rom,
            backup,

            dma: [0, 1, 2, 3].map(|ch| Dma::new(ch)),
            timers: Default::default(),

            key_input: 0x3FF,
            key_interrupt_spec: 0,
            key_interrupt_enable: false,
            key_interrupt_cond: false,

            sio: Serial::default(),

            game_pak_ram_wait_ctrl,
            game_pak_wait_ctrl,

            phi_terminal_output_ctrl: 0,
            prefetch_buffer: false,
            game_pak_type: false,

            post_boot: 0,

            prefetch_base: 0,
            prefetch_start_time: 0,
            prefetched_size: 0,

            wait_cycles,
        }
    }

    pub fn set_key_input(&mut self, ctx: &mut impl Context, key_input: &KeyInput) {
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

        if self.key_interrupt_enable {
            let b = if !self.key_interrupt_cond {
                // OR
                (!self.key_input & self.key_interrupt_spec) != 0
            } else {
                // AND
                (!self.key_input & self.key_interrupt_spec) == self.key_interrupt_spec
            };
            if b {
                ctx.interrupt_mut().set_interrupt(InterruptKind::Keypad);
            }
        }
    }

    pub fn dma(&self, ch: usize) -> &Dma {
        &self.dma[ch]
    }

    pub fn dma_mut(&mut self, ch: usize) -> &mut Dma {
        &mut self.dma[ch]
    }

    pub fn backup(&self) -> &Backup {
        &self.backup
    }

    pub fn backup_mut(&mut self) -> &mut Backup {
        &mut self.backup
    }

    pub fn tick(&mut self, ctx: &mut impl Context) {
        for ch in 0..4 {
            self.process_dma(ctx, ch);
        }

        self.timers.tick(ctx);
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
                let ofs = addr & 1;
                (self.read_rom(ctx, addr & !1, first) >> (ofs * 8)) as u8
            }

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_8);
                self.backup.read_ram(addr & 0xFFFF)
            }

            _ => {
                warn!("Invalid Read8: 0x{addr:08X}");
                0
            }
        }
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32, first: bool) -> u16 {
        // trace!("Read16: 0x{addr:08X}");

        match addr >> 24 {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    ctx.elapse(1);
                    read16(&self.bios, (addr & !1) as usize)
                } else {
                    warn!("Invalid BIOS address read: 0x{addr:08X}:16");
                    0
                }
            }
            0x2 => {
                ctx.elapse(3);
                read16(&self.ext_ram, (addr & 0x3FFFE) as usize)
            }
            0x3 => {
                ctx.elapse(1);
                read16(&self.ram, (addr & 0x7FFE) as usize)
            }

            0x4 => {
                ctx.elapse(1);
                let data = self.io_read16(ctx, addr & 0xFFFE);
                trace_io::<u16, true>(addr, data);
                data
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(1);
                read16(&ctx.lcd().palette, (addr & 0x3FE) as usize)
            }
            0x6 => {
                ctx.elapse(1);
                read16(&ctx.lcd().vram, vram_addr(addr & !1))
            }
            0x7 => {
                ctx.elapse(1);
                read16(&ctx.lcd().oam, (addr & 0x3FE) as usize)
            }

            0x8..=0xD => self.read_rom(ctx, addr & !1, first),

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_16);
                let lo = self.backup.read_ram(addr & 0xFFFF);
                (lo as u16) << 8 | lo as u16
            }

            _ => {
                warn!("Invalid Read16: 0x{addr:08X}");
                0
            }
        }
    }

    pub fn read32(&mut self, ctx: &mut impl Context, addr: u32, first: bool) -> u32 {
        // trace!("Read32: 0x{addr:08X}");

        match addr >> 24 {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    ctx.elapse(1);
                    read32(&self.bios, (addr & !3) as usize)
                } else {
                    warn!("Invalid BIOS address read: 0x{addr:08X}:32");
                    0
                }
            }
            0x2 => {
                ctx.elapse(6);
                read32(&self.ext_ram, (addr & 0x3FFFC) as usize)
            }
            0x3 => {
                ctx.elapse(1);
                read32(&self.ram, (addr & 0x7FFC) as usize)
            }

            0x4 => {
                ctx.elapse(1);
                let addr = addr & 0xFFFC;
                let lo = self.io_read16(ctx, addr);
                let hi = self.io_read16(ctx, addr + 2);
                let data = (hi as u32) << 16 | lo as u32;
                trace_io::<u32, true>(addr, data);
                data
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(2);
                read32(&ctx.lcd().palette, (addr & 0x3FC) as usize)
            }
            0x6 => {
                ctx.elapse(2);
                read32(&ctx.lcd().vram, vram_addr(addr & !3))
            }
            0x7 => {
                ctx.elapse(1);
                read32(&ctx.lcd().oam, (addr & 0x3FC) as usize)
            }

            0x8..=0xD => {
                let lo = self.read_rom(ctx, addr & !3, first);
                let hi = self.read_rom(ctx, (addr & !3) + 2, false);
                ((hi as u32) << 16) | lo as u32
            }

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_32);
                let lo = self.backup.read_ram(addr & 0xFFFF);
                (lo as u32) << 24 | (lo as u32) << 16 | (lo as u32) << 8 | lo as u32
            }
            _ => {
                warn!("Invalid Read32: 0x{addr:08X}");
                0
            }
        }
    }

    fn read_rom(&mut self, ctx: &mut impl Context, addr: u32, first: bool) -> u16 {
        let ws = (addr >> 25) as usize - 4;

        let wc = if first {
            self.wait_cycles.gamepak_rom_1st_16[ws]
        } else {
            self.wait_cycles.gamepak_rom_2nd_16[ws]
        };

        if self.is_valid_eeprom_addr(addr) {
            ctx.elapse(wc);
            return self.backup.read_eeprom() as u16;
        }

        let addr = addr & 0x01FFFFFE;

        if (addr as usize) >= self.rom.data.len() {
            ctx.elapse(wc);
            warn!("Write to invalid Game Pak ROM address: {addr:08X}");
            return 0;
        }

        if !self.prefetch_buffer {
            ctx.elapse(wc);
        } else {
            // FIXME: This is too buggy

            let now = ctx.now();
            let elapsed = now - self.prefetch_start_time;
            let fetched = elapsed / self.wait_cycles.gamepak_rom_2nd_16[ws];
            self.prefetched_size = min(8, self.prefetched_size as u64 + fetched) as u32;
            self.prefetch_start_time = if self.prefetched_size == 8 {
                now
            } else {
                self.prefetch_start_time + fetched * self.wait_cycles.gamepak_rom_2nd_16[ws]
            };

            // FIXME: address overflow
            if self.prefetch_base <= addr
                && addr < self.prefetch_base.wrapping_add(self.prefetched_size * 2)
            {
                self.prefetch_base = self.prefetch_base.wrapping_add(2);
                self.prefetched_size -= 1;
                ctx.elapse(1);

                // trace!("Prefetch hit:  time: {now}, addr=0x{addr:08X}");
            } else {
                if addr == self.prefetch_base {
                    // trace!("Prefetch miss: time: {now}, addr=0x{addr:08X}, seq");
                    ctx.elapse(self.wait_cycles.gamepak_rom_2nd_16[ws]);
                } else {
                    // trace!("Prefetch miss: time: {now}, addr=0x{addr:08X}, non-seq");
                    ctx.elapse(self.wait_cycles.gamepak_rom_1st_16[ws]);
                }
                self.prefetch_base = addr.wrapping_add(2);
                self.prefetched_size = 0;
                self.prefetch_start_time = now;
            }
        }

        read16(&self.rom.data, addr as usize)
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8, first: bool) {
        // trace!("Write8: 0x{addr:08X} = 0x{data:02X}");

        match addr >> 24 {
            0x0..=0x1 => {
                // FIXME: ???
                warn!("Write to BIOS: 0x{addr:08X}:8");
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
                let addr = (addr & 0x3FE) as usize;
                ctx.lcd_mut().palette[addr] = data;
                ctx.lcd_mut().palette[addr + 1] = data;
            }
            0x6 => {
                warn!("Write 8bit data to VRAM: 0x{addr:08X} = 0x{data:02X}");
                ctx.elapse(1);
                let addr = vram_addr(addr);
                if addr < 0x10000 {
                    ctx.lcd_mut().vram[addr] = data;
                    ctx.lcd_mut().vram[addr + 1] = data;
                }
            }
            0x7 => {
                warn!("Write 8bit data to OAM: 0x{addr:08X} = 0x{data:02X}");
                ctx.elapse(1);
                // This seems to be ignored
            }

            0x8..=0xD => warn!("Write 8bit data to ROM: 0x{addr:08X} = 0x{data:02X}"),

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_8);
                self.backup.write_ram(addr & 0xFFFF, data);
            }
            _ => panic!("Write8: 0x{addr:08X} = 0x{data:02X}"),
        }
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16, first: bool) {
        // trace!("Write16: 0x{addr:08X} = 0x{data:04X}");

        match addr >> 24 {
            0x0..=0x1 => {
                warn!("Write to BIOS: 0x{addr:08X}:16");
            }
            0x2 => {
                ctx.elapse(3);
                write16(&mut self.ext_ram, (addr & 0x3FFFE) as usize, data);
            }
            0x3 => {
                ctx.elapse(1);
                write16(&mut self.ram, (addr & 0x7FFE) as usize, data);
            }

            0x4 => {
                ctx.elapse(1);
                trace_io::<u16, false>(addr, data);
                self.io_write16(ctx, addr & 0xFFFE, data);
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(1);
                write16(&mut ctx.lcd_mut().palette, (addr & 0x3FE) as usize, data);
            }
            0x6 => {
                ctx.elapse(1);
                write16(&mut ctx.lcd_mut().vram, vram_addr(addr & !1), data);
            }
            0x7 => {
                ctx.elapse(1);
                write16(&mut ctx.lcd_mut().oam, (addr & 0x3FE) as usize, data);
            }

            0x8..=0xD => {
                let ix = (addr >> 25) as usize - 4;
                ctx.elapse(if first {
                    self.wait_cycles.gamepak_rom_1st_16[ix]
                } else {
                    self.wait_cycles.gamepak_rom_2nd_16[ix]
                });

                if self.is_valid_eeprom_addr(addr) {
                    self.backup.write_eeprom(data & 1 != 0);
                } else {
                    warn!("Write to invalid Game Pak ROM address: {addr:08X}");
                }
            }

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_16);
                self.backup.write_ram(addr & 0xFFFF, data as u8);
                self.backup
                    .write_ram((addr + 1) & 0xFFFF, (data >> 8) as u8);
            }
            _ => panic!("Write16: 0x{addr:08X} = 0x{data:04X}"),
        }
    }

    pub fn write32(&mut self, ctx: &mut impl Context, addr: u32, data: u32, first: bool) {
        // trace!("Write32: 0x{addr:08X} = 0x{data:08X}");

        match addr >> 24 {
            0x0..=0x1 => {
                warn!("Write to BIOS: 0x{addr:08X}:32");
            }
            0x2 => {
                ctx.elapse(6);
                write32(&mut self.ext_ram, (addr & 0x3FFFC) as usize, data);
            }
            0x3 => {
                ctx.elapse(1);
                write32(&mut self.ram, (addr & 0x7FFC) as usize, data);
            }

            0x4 => {
                ctx.elapse(1);
                trace_io::<u32, false>(addr, data);
                let addr = addr & 0xFFFC;
                self.io_write16(ctx, addr, data as u16);
                self.io_write16(ctx, addr + 2, (data >> 16) as u16);
            }

            // TODO: Plus 1 cycle if GBA accesses video memory at the same time.
            0x5 => {
                ctx.elapse(2);
                write32(&mut ctx.lcd_mut().palette, (addr & 0x3FC) as usize, data);
            }
            0x6 => {
                ctx.elapse(2);
                write32(&mut ctx.lcd_mut().vram, vram_addr(addr & !3), data);
            }
            0x7 => {
                ctx.elapse(1);
                write32(&mut ctx.lcd_mut().oam, (addr & 0x3FC) as usize, data);
            }

            0x8..=0xD => warn!("Write 32bit data to ROM: 0x{addr:08X} = 0x{data:08X}"),

            0xE..=0xF => {
                ctx.elapse(self.wait_cycles.gamepak_ram_32);
                self.backup.write_ram(addr & 0xFFFF, data as u8);
                self.backup
                    .write_ram((addr + 1) & 0xFFFF, (data >> 8) as u8);
                self.backup
                    .write_ram((addr + 2) & 0xFFFF, (data >> 16) as u8);
                self.backup
                    .write_ram((addr + 3) & 0xFFFF, (data >> 24) as u8);
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
            0x000..=0x05E => ctx.lcd_read(addr),
            0x060..=0x0AF => ctx.sound_read(addr),

            _ => {
                let data = self.io_read16(ctx, addr & !1);
                (data >> ((addr & 1) * 8)) as u8
            }
        }
    }

    pub fn io_read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        match addr {
            0x000..=0x05E => {
                let lo = ctx.lcd_read(addr);
                let hi = ctx.lcd_read(addr + 1);
                (hi as u16) << 8 | lo as u16
            }
            0x060..=0x0AE => {
                let lo = ctx.sound_read(addr);
                let hi = ctx.sound_read(addr + 1);
                (hi as u16) << 8 | lo as u16
            }
            0x0B0..=0x0DE => self.read_dma16(addr),
            0x0E0..=0x0FE => 0,
            0x100..=0x10E => self.timers.read16(addr),

            // SIOCNT
            0x128 => {
                // TODO
                warn!("Read SIOCNT");
                0
            }
            // SIODATA
            0x12A => {
                warn!("Read SIODATA8");
                0
            }

            // KEYINPUT
            0x130 => self.key_input,

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

            // IME
            0x208 => ctx.interrupt_mut().master_enable() as u16,
            0x20A => 0,

            // POSTFLG
            0x300 => self.post_boot as u16,

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
            0x000..=0x05F => ctx.lcd_write(addr, data),
            0x060..=0x0AF => ctx.sound_write(addr, data),

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
                    debug!("Enter halt mode");
                    ctx.interrupt_mut().set_halt(true);
                } else if data == 0x80 {
                    // FIXME
                    todo!("Enter stop mode");
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
            0x000..=0x05E => {
                ctx.lcd_write(addr, data as u8);
                ctx.lcd_write(addr + 1, (data >> 8) as u8);
            }
            0x060..=0x0AE => {
                ctx.sound_write(addr, data as u8);
                ctx.sound_write(addr + 1, (data >> 8) as u8);
            }
            0x0B0..=0x0DE => self.write_dma16(ctx, addr, data),
            0x0E0..=0x0FE => {}
            0x100..=0x10E => self.timers.write16(addr, data),
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

                debug!(
                    "WaitCycles: prefetch: {}, ram: {}, rom: {:?}, {:#?}",
                    self.prefetch_buffer,
                    self.game_pak_ram_wait_ctrl,
                    self.game_pak_wait_ctrl,
                    self.wait_cycles
                );
            }

            0x206 | 0x20A | 0x20C..=0x21E | 0x100C => {}

            _ => {
                self.io_write8(ctx, addr, data as u8);
                self.io_write8(ctx, addr + 1, (data >> 8) as u8);
            }
        }
    }

    pub fn is_valid_eeprom_addr(&self, addr: u32) -> bool {
        if addr & 0x08000000 == 0 {
            return false;
        }
        let ofs = addr & 0x01FFFFFF;
        let large_rom = self.rom.data.len() >= 0x01000000;
        (!large_rom && ofs & 0x01000000 != 0) || (large_rom && ofs & 0x01FFFF00 == 0x01FFFF00)
    }
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
