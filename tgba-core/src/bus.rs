use std::{fmt::UpperHex, mem::size_of};

use bitvec::prelude::*;
use log::{info, trace, warn};

use crate::{
    context::{Interrupt, Lcd, Sound},
    rom::Rom,
    util::{pack, trait_alias},
};

trait_alias!(pub trait Context = Lcd + Sound + Interrupt);

pub struct Bus {
    bios: Vec<u8>,
    rom: Rom,
    ram: Vec<u8>,
    ext_ram: Vec<u8>,

    dma: [Dma; 4],
    timer: [Timer; 4],

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

    reg_width_r: Vec<usize>,
    reg_width_w: Vec<usize>,
}

#[derive(Default)]
struct Dma {
    source_addr: u32,
    dest_addr: u32,
    word_count: u16,

    source_addr_ctrl: u8,
    dest_addr_ctrl: u8,
    repeat: bool,
    transfer_type: bool, // 0: 16bit, 1: 32bit
    game_pak_data_request_transfer: bool,

    // 00: Start immediately
    // 01: Start in a V-blank interval
    // 10: Start in an H-blank interval
    // 11: Prohibited
    start_timing: u8,
    irq_enable: bool,
    dma_enable: bool,
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

enum Key {
    A = 0,
    B = 1,
    Select = 2,
    Start = 3,
    Right = 4,
    Left = 5,
    Up = 6,
    Down = 7,
    R = 8,
    L = 9,
}

impl Bus {
    pub fn new(bios: Vec<u8>, rom: Rom) -> Self {
        let ram = vec![0; 0x8000];
        let ext_ram = vec![0; 0x40000];

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

        Bus {
            bios,
            rom,
            ram,
            ext_ram,

            dma: Default::default(),
            timer: Default::default(),

            key_interrupt_spec: 0,
            key_interrupt_enable: false,
            key_interrupt_cond: false,

            sio: Default::default(),

            game_pak_ram_wait_ctrl: 0,
            game_pak_wait_ctrl: [0; 3],
            phi_terminal_output_ctrl: 0,
            prefetch_buffer: false,
            game_pak_type: false,

            post_boot: 0,

            reg_width_r,
            reg_width_w,
        }
    }

    pub fn game_pak_ram_wait_cycle(&self) -> u64 {
        const TBL: [u64; 4] = [4, 3, 2, 8];
        TBL[self.game_pak_ram_wait_ctrl as usize]
    }

    pub fn game_pak_wait_cycle(&self, ix: usize, first: bool) -> u64 {
        const FIRST: [u64; 8] = [4, 3, 2, 8, 4, 3, 2, 8];
        const SECOND: [[u64; 8]; 3] = [
            [2, 2, 2, 2, 1, 1, 1, 1],
            [4, 4, 4, 4, 1, 1, 1, 1],
            [8, 8, 8, 8, 1, 1, 1, 1],
        ];

        let ctrl = self.game_pak_wait_ctrl[ix] as usize;

        if first {
            FIRST[ctrl]
        } else {
            SECOND[ix][ctrl]
        }
    }

    pub fn read8(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        // trace!("Read8: 0x{addr:08X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    self.bios[addr as usize]
                } else {
                    panic!()
                }
            }
            0x2 => self.ext_ram[(addr & 0x3FFFF) as usize],
            0x3 => self.ram[(addr & 0x7FFF) as usize],

            0x4 => {
                let data = self.io_read8(ctx, addr & 0xFFFF);
                trace_io::<u8, true>(addr, data);
                data
            }

            0x5 => panic!("Read 8bit data from Palette"),
            0x6 => panic!("Read 8bit data from VRAM"),
            0x7 => panic!("Read 8bit data from OAM"),

            0x8..=0x9 => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    self.rom.data[ofs]
                } else {
                    panic!()
                }
            }
            0xA..=0xB => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    self.rom.data[ofs]
                } else {
                    panic!()
                }
            }
            0xC..=0xD => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    self.rom.data[ofs]
                } else {
                    panic!()
                }
            }
            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
        }
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        // trace!("Read16: 0x{addr:08X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    read16(&self.bios, addr as usize)
                } else {
                    panic!()
                }
            }
            0x2 => read16(&self.ext_ram, (addr & 0x3FFFF) as usize),
            0x3 => read16(&self.ram, (addr & 0x7FFF) as usize),

            0x4 => {
                let data = self.io_read16(ctx, addr & 0xFFFF);
                trace_io::<u16, true>(addr, data);
                data
            }

            0x5 => read16(&ctx.lcd().palette, (addr & 0x3FF) as usize),
            0x6 => read16(&ctx.lcd().vram, vram_addr(addr)),
            0x7 => read16(&ctx.lcd().oam, (addr & 0x3FF) as usize),

            0x8..=0x9 => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read16(&self.rom.data, ofs)
                } else {
                    panic!()
                }
            }
            0xA..=0xB => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read16(&self.rom.data, ofs)
                } else {
                    panic!("{addr:08X}");
                }
            }
            0xC..=0xD => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read16(&self.rom.data, ofs)
                } else {
                    panic!()
                }
            }
            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
        }
    }

    pub fn read32(&mut self, ctx: &mut impl Context, addr: u32) -> u32 {
        // trace!("Read32: 0x{addr:08X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    read32(&self.bios, addr as usize)
                } else {
                    panic!()
                }
            }
            0x2 => read32(&self.ext_ram, (addr & 0x3FFFF) as usize),
            0x3 => read32(&self.ram, (addr & 0x7FFF) as usize),

            0x4 => {
                let addr = addr & 0xFFFF;
                let lo = self.io_read16(ctx, addr);
                let hi = self.io_read16(ctx, addr + 2);
                let data = (hi as u32) << 16 | lo as u32;
                trace_io::<u32, true>(addr, data);
                data
            }

            0x5 => read32(&ctx.lcd().palette, (addr & 0x3FF) as usize),
            0x6 => read32(&ctx.lcd().vram, vram_addr(addr)),
            0x7 => read32(&ctx.lcd().oam, (addr & 0x3FF) as usize),

            0x8..=0x9 => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read32(&self.rom.data, ofs)
                } else {
                    panic!()
                }
            }
            0xA..=0xB => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read32(&self.rom.data, ofs)
                } else {
                    panic!()
                }
            }
            0xC..=0xD => {
                let ofs = (addr & 0x01FFFFFF) as usize;
                if ofs < self.rom.data.len() {
                    read32(&self.rom.data, ofs)
                } else {
                    panic!()
                }
            }
            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
        }
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        // trace!("Write8: 0x{addr:08X} = 0x{data:02X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                // FIXME: ???
                panic!("Write to BIOS");
            }
            0x2 => self.ext_ram[(addr & 0x3FFFF) as usize] = data,
            0x3 => self.ram[(addr & 0x7FFF) as usize] = data,

            0x4 => {
                trace_io::<u8, false>(addr, data);
                self.io_write8(ctx, addr & 0xFFFF, data);
            }

            0x5 => panic!("Write 8bit data to Palette"),
            0x6 => panic!("Write 8bit data to VRAM"),
            0x7 => panic!("Write 8bit data to OAM"),

            0x8..=0x9 => panic!("Write 8bit data to ROM"),
            0xA..=0xB => panic!("Write 8bit data to ROM"),
            0xC..=0xD => panic!("Write 8bit data to ROM"),

            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
        }
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        // trace!("Write16: 0x{addr:08X} = 0x{data:04X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                panic!("Write to BIOS");
            }
            0x2 => write16(&mut self.ext_ram, (addr & 0x3FFFF) as usize, data),
            0x3 => write16(&mut self.ram, (addr & 0x7FFF) as usize, data),

            0x4 => {
                trace_io::<u16, false>(addr, data);
                self.io_write16(ctx, addr & 0xFFFF, data);
            }

            0x5 => write16(&mut ctx.lcd_mut().palette, (addr & 0x3FF) as usize, data),
            0x6 => write16(&mut ctx.lcd_mut().vram, vram_addr(addr), data),
            0x7 => write16(&mut ctx.lcd_mut().oam, (addr & 0x3FF) as usize, data),

            0x8..=0x9 => panic!("Write 8bit data to ROM"),
            0xA..=0xB => panic!("Write 8bit data to ROM"),
            0xC..=0xD => panic!("Write 8bit data to ROM"),

            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
        }
    }

    pub fn write32(&mut self, ctx: &mut impl Context, addr: u32, data: u32) {
        // trace!("Write32: 0x{addr:08X} = 0x{data:08X}");

        match (addr >> 24) & 0xF {
            0x0..=0x1 => {
                if addr < 0x00004000 {
                    write32(&mut self.bios, addr as usize, data)
                } else {
                    panic!()
                }
            }
            0x2 => write32(&mut self.ext_ram, (addr & 0x3FFFF) as usize, data),
            0x3 => write32(&mut self.ram, (addr & 0x7FFF) as usize, data),

            0x4 => {
                trace_io::<u32, false>(addr, data);
                let addr = addr & 0xFFFF;
                self.io_write16(ctx, addr, data as u16);
                self.io_write16(ctx, addr + 2, (data >> 16) as u16);
            }

            0x5 => write32(&mut ctx.lcd_mut().palette, (addr & 0x3FF) as usize, data),
            0x6 => write32(&mut ctx.lcd_mut().vram, vram_addr(addr), data),
            0x7 => write32(&mut ctx.lcd_mut().oam, (addr & 0x3FF) as usize, data),

            0x8..=0x9 => panic!("Write 8bit data to ROM"),
            0xA..=0xB => panic!("Write 8bit data to ROM"),
            0xC..=0xD => panic!("Write 8bit data to ROM"),

            0xE..=0xF => {
                todo!("GamePak RAM")
            }
            _ => unreachable!(),
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
                // TODO: is this correct?
                warn!("Read 8bit data from LCD register: 0x{addr:08X}");
                let data = ctx.lcd_read16(addr & !1);
                (data >> ((addr & 1) * 8)) as u8
            }
            0x060..=0x0AF => ctx.sound_read8(addr),

            // POSTFLG
            0x300 => self.post_boot,
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
                    "Writing 8bit data to LCD registers are ignored: 0x{addr:08X} = 0x{data:02X}"
                );
            }
            0x060..=0x0AF => ctx.sound_write8(addr, data),

            // JOYCNT
            0x140 => {
                info!("JOYCNT = 0x{data:02X}");
            }
            0x141..=0x14F => {}

            // IME
            0x208 => ctx.interrupt_mut().set_master_enable((data & 1) != 0),

            // POSTFLG
            0x300 => self.post_boot = data,

            // ???
            0x410 => {}

            0x209 => {}

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
                self.dma[i].source_addr.view_bits_mut::<Lsb0>()[0..=15].store(data);
            }
            0x0B2 | 0x0BE | 0x0CA | 0x0D6 => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                let hi = if i == 0 { 26 } else { 27 };
                self.dma[i].source_addr.view_bits_mut::<Lsb0>()[16..=hi].store(data);
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
                self.dma[i].word_count = data & mask;
            }
            0x0BA | 0x0C6 | 0x0D2 | 0x0DE => {
                let i = ((addr - 0x0B0) / 0xC) as usize;
                let dma = &mut self.dma[i];

                let v = data.view_bits::<Lsb0>();
                dma.dest_addr_ctrl = v[5..=6].load();
                dma.source_addr_ctrl = v[7..=8].load();
                dma.repeat = v[9];
                dma.transfer_type = v[10];
                if i == 3 {
                    dma.game_pak_data_request_transfer = v[11];
                }
                dma.start_timing = v[12..=13].load();
                dma.irq_enable = v[14];
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
                }
                timer.enable = data[7];
            }

            0x110..=0x11E => {}

            // SIODATA
            0x120 | 0x122 | 0x124 | 0x126 => self.sio.data[((addr - 0x120) / 2) as usize] = data,

            // SIOCNT
            0x128 => {
                let data = data.view_bits::<Lsb0>();

                match data[12..13].load::<u8>() {
                    _ => {
                        // TODO
                        info!("SIOCNT = 0x{:05X}", data.load::<u16>());
                    }
                }
            }

            // SIODATA8
            0x12A => self.sio.data8 = data as u8,

            0x12C | 0x12E => {}

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
            0x202 => ctx.interrupt_mut().reset_request(data & 0x3FFF),

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
            }

            0x206 | 0x20A | 0x20C..=0x21E => {}

            _ => {
                self.io_write8(ctx, addr, data as u8);
                self.io_write8(ctx, addr + 1, (data >> 8) as u8);
            }
        }
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
    // 0x4000122  2    R/W  SIOMULTI1 "SIO Data 1 (1st Child) (Multi-Player Mode)"
    0x4000124  2    R/W  SIOMULTI2 "SIO Data 2 (2nd Child) (Multi-Player Mode)"
    0x4000126  2    R/W  SIOMULTI3 "SIO Data 3 (3rd Child) (Multi-Player Mode)"
    0x4000128  2    R/W  SIOCNT    "SIO Control Register"
    0x400012A  2    R/W  SIOMLT_SEND "SIO Data (Local of MultiPlayer; shared below)"
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
    // 0x4700000  4    W    (3DS)     "Disable ARM7 bootrom overlay (3DS only)"

    @end
};
