use bitvec::prelude::*;
use log::{info, trace};
use std::mem::size_of;

use crate::{
    context::{Bus, Interrupt, Timing},
    util::trait_alias,
};

trait_alias!(pub trait Context = Bus + Timing + Interrupt);

type ArmOp<C> = fn(&mut Cpu<C>, &mut C, u32);
type ArmDisasm = fn(u32, u32) -> String;

type ThumbOp<C> = fn(&mut Cpu<C>, &mut C, u16);
type ThumbDisasm = fn(u16, u32) -> String;

pub struct Cpu<C: Context> {
    regs: Registers,
    pc_changed: bool,
    fetch_first: bool,

    prev_regs: [u32; 16],

    trace: bool,

    arm_op_table: [ArmOp<C>; 0x1000],
    arm_disasm_table: [ArmDisasm; 0x1000],

    thumb_op_table: [ThumbOp<C>; 0x400],
    thumb_disasm_table: [ThumbDisasm; 0x400],
}

#[derive(Debug)]
enum Exception {
    Reset,
    UndefinedInstruction,
    SoftwareInterrupt,
    PrefetchAbort,
    DataAbort,
    // Reserved,
    IRQ,
    FIQ,
}

impl Exception {
    fn vector_addr(&self) -> u32 {
        match self {
            Exception::Reset => 0x00000000,
            Exception::UndefinedInstruction => 0x00000004,
            Exception::SoftwareInterrupt => 0x00000008,
            Exception::PrefetchAbort => 0x0000000C,
            Exception::DataAbort => 0x00000010,
            // Exception::Reserved => 0x00000014,
            Exception::IRQ => 0x00000018,
            Exception::FIQ => 0x0000001C,
        }
    }

    fn mode_on_entry(&self) -> u8 {
        match self {
            Exception::Reset => MODE_SUPERVISOR,
            Exception::UndefinedInstruction => MODE_UNDEFINED,
            Exception::SoftwareInterrupt => MODE_SUPERVISOR,
            Exception::PrefetchAbort => MODE_ABORT,
            Exception::DataAbort => MODE_ABORT,
            Exception::IRQ => MODE_IRQ,
            Exception::FIQ => MODE_FIQ,
        }
    }
}

const MODE_USER: u8 = 0b10000;
const MODE_FIQ: u8 = 0b10001;
const MODE_IRQ: u8 = 0b10010;
const MODE_SUPERVISOR: u8 = 0b10011;
const MODE_ABORT: u8 = 0b10111;
const MODE_UNDEFINED: u8 = 0b11011;
const MODE_SYSTEM: u8 = 0b11111;

fn mode_name(mode: u8) -> &'static str {
    match mode {
        MODE_USER => "USR",
        MODE_FIQ => "FIQ",
        MODE_IRQ => "IRQ",
        MODE_SUPERVISOR => "SVC",
        MODE_ABORT => "ABT",
        MODE_UNDEFINED => "UND",
        MODE_SYSTEM => "SYS",
        _ => "INV",
    }
}

struct Registers {
    r: [u32; 16],

    n_flag: bool,
    z_flag: bool,
    c_flag: bool,
    v_flag: bool,
    irq_disable: bool,
    fiq_disable: bool,
    state: bool,
    mode: u8,

    spsr: u32,

    gprs: [u32; 31],
    spsrs: [u32; 5],
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            r: [0; 16],

            n_flag: false,
            z_flag: false,
            c_flag: false,
            v_flag: false,
            irq_disable: true,
            fiq_disable: true,
            state: false,
            mode: MODE_SUPERVISOR,

            spsr: 0,

            gprs: [0; 31],
            spsrs: [0; 5],
        }
    }
}

impl Registers {
    fn cpsr(&self) -> u32 {
        let mut ret = 0;
        ret |= (self.n_flag as u32) << 31;
        ret |= (self.z_flag as u32) << 30;
        ret |= (self.c_flag as u32) << 29;
        ret |= (self.v_flag as u32) << 28;
        ret |= (self.irq_disable as u32) << 7;
        ret |= (self.fiq_disable as u32) << 6;
        ret |= (self.state as u32) << 5;
        ret |= self.mode as u32;
        ret
    }

    fn set_cpsr(&mut self, cpsr: u32) {
        self.n_flag = (cpsr >> 31) & 1 != 0;
        self.z_flag = (cpsr >> 30) & 1 != 0;
        self.c_flag = (cpsr >> 29) & 1 != 0;
        self.v_flag = (cpsr >> 28) & 1 != 0;
        self.irq_disable = (cpsr >> 7) & 1 != 0;
        self.fiq_disable = (cpsr >> 6) & 1 != 0;
        self.state = (cpsr >> 5) & 1 != 0;

        self.change_mode((cpsr & 0b11111) as u8);
    }

    fn is_valid_mode(&self) -> bool {
        matches!(
            self.mode,
            MODE_USER
                | MODE_FIQ
                | MODE_IRQ
                | MODE_SUPERVISOR
                | MODE_ABORT
                | MODE_UNDEFINED
                | MODE_SYSTEM
        )
    }

    fn change_mode(&mut self, mode: u8) {
        if self.mode != mode {
            self.save_regs();
            self.mode = mode;
            assert!(self.is_valid_mode());
            self.restore_regs();
        }
    }

    fn save_regs(&mut self) {
        let (gprs, spsr) = reg_bank(self.mode);

        for i in 0..16 {
            self.gprs[gprs[i]] = self.r[i];
        }
        if let Some(spsr) = spsr {
            self.spsrs[spsr] = self.spsr;
        }
    }

    fn restore_regs(&mut self) {
        let (gprs, spsr) = reg_bank(self.mode);

        for i in 0..16 {
            self.r[i] = self.gprs[gprs[i]];
        }
        if let Some(spsr) = spsr {
            self.spsr = self.spsrs[spsr];
        }
    }

    fn set_nz(&mut self, v: u32) {
        self.n_flag = v >> 31 != 0;
        self.z_flag = v == 0;
    }

    fn check_cond(&self, cond: u8) -> bool {
        match cond {
            0b0000 => self.z_flag,                                // EQ
            0b0001 => !self.z_flag,                               // NE
            0b0010 => self.c_flag,                                // CS
            0b0011 => !self.c_flag,                               // CC
            0b0100 => self.n_flag,                                // MI
            0b0101 => !self.n_flag,                               // PL
            0b0110 => self.v_flag,                                // VS
            0b0111 => !self.v_flag,                               // VC
            0b1000 => self.c_flag && !self.z_flag,                // HI
            0b1001 => !self.c_flag || self.z_flag,                // LS
            0b1010 => self.n_flag == self.v_flag,                 // GE
            0b1011 => self.n_flag != self.v_flag,                 // LT
            0b1100 => !self.z_flag && self.n_flag == self.v_flag, // GT
            0b1101 => self.z_flag || self.n_flag != self.v_flag,  // LE
            0b1110 => true,                                       // AL
            _ => panic!("invalid condition code: {cond:04b}"),
        }
    }
}

fn reg_bank(mode: u8) -> (&'static [usize; 16], Option<usize>) {
    const GPRS: [[usize; 16]; 6] = [
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
        [0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 15],
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 23, 24, 15],
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 25, 26, 15],
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 27, 28, 15],
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 29, 30, 15],
    ];

    const SPSRS: [Option<usize>; 6] = [None, Some(0), Some(1), Some(2), Some(3), Some(4)];

    let ix = match mode {
        MODE_USER | MODE_SYSTEM => 0,
        MODE_FIQ => 1,
        MODE_SUPERVISOR => 2,
        MODE_ABORT => 3,
        MODE_IRQ => 4,
        MODE_UNDEFINED => 5,
        _ => panic!("Change to invalid mode: {mode}"),
    };

    (&GPRS[ix], SPSRS[ix])
}

impl<C: Context> Cpu<C> {
    pub fn new() -> Self {
        let (arm_op_table, arm_disasm_table) = build_arm_table();
        let (thumb_op_table, thumb_disasm_table) = build_thumb_table();

        Cpu {
            regs: Registers::default(),
            pc_changed: false,
            fetch_first: true,
            prev_regs: [0; 16],
            trace: false,
            arm_op_table,
            arm_disasm_table,
            thumb_op_table,
            thumb_disasm_table,
        }
    }

    pub fn set_pc(&mut self, pc: u32) {
        self.regs.r[15] = pc;
        self.pc_changed = true;
        self.fetch_first = true;
    }

    pub fn exec_one(&mut self, ctx: &mut C) {
        // static mut PREV: u64 = 0;
        // if ctx.now() - unsafe { PREV } > 10000 {
        //     unsafe { PREV = ctx.now() };
        //     trace!("HB: {}", ctx.now());
        // }

        // if ctx.now() > 77162629 {
        //     self.trace = true;
        // }

        if !self.regs.fiq_disable && ctx.interrupt().fiq() {
            ctx.interrupt_mut().set_halt(false);
            self.exception(Exception::FIQ);
            // FIXME
            ctx.elapse(28);
            return;
        }

        if !self.regs.irq_disable && ctx.interrupt().irq() {
            info!("IRQ occured: cycle {}", ctx.now());

            ctx.interrupt_mut().set_halt(false);
            self.exception(Exception::IRQ);
            // FIXME: correct time
            ctx.elapse(28);
            return;
        }

        if ctx.interrupt().halt() {
            ctx.elapse(1);
            return;
        }

        if !self.regs.state {
            self.exec_arm(ctx);
        } else {
            self.exec_thumb(ctx);
        }
    }

    fn exception(&mut self, e: Exception) {
        info!("Exception: {e:?}");

        let old_cpsr = self.regs.cpsr();

        let old_pc = if matches!(e, Exception::SoftwareInterrupt) {
            // On SWI, saved pc should be next instruction
            if !self.regs.state {
                // In arm mode, PC is +8 to SWI instruction
                self.regs.r[15].wrapping_sub(4)
            } else {
                // In thumb mode, PC is +2 to SWI instruction
                self.regs.r[15]
            }
        } else {
            if matches!(e, Exception::DataAbort) {
                self.regs.r[15].wrapping_add(8)
            } else {
                self.regs.r[15].wrapping_add(4)
            }
        };

        self.regs.change_mode(e.mode_on_entry());
        self.regs.spsr = old_cpsr;
        self.regs.r[14] = old_pc;
        self.regs.r[15] = e.vector_addr();
        self.regs.state = false;
        self.regs.irq_disable = true;
        self.regs.fiq_disable = true;
        self.pc_changed = true;
        self.fetch_first = true;
    }

    fn exec_arm(&mut self, ctx: &mut C) {
        let instr = self.fetch32(ctx);

        if self.trace && log::log_enabled!(log::Level::Trace) {
            let pc = self.regs.r[15].wrapping_sub(4);
            let s = self.disasm_arm(instr, pc);
            let regs = self.dump_regs();
            trace!("{pc:08X}: {instr:08X}: {s:24} {regs}");
        }

        if self.regs.check_cond((instr >> 28) as u8) {
            self.regs.r[15] = self.regs.r[15].wrapping_add(4);
            self.pc_changed = false;

            let ix = (instr >> 16) & 0xFF0 | (instr >> 4) & 0xF;
            self.arm_op_table[ix as usize](self, ctx, instr);

            if !self.pc_changed {
                self.regs.r[15] = self.regs.r[15].wrapping_sub(4);
            } else {
                self.fetch_first = true;
            }
        }
    }

    fn exec_thumb(&mut self, ctx: &mut C) {
        let instr = self.fetch16(ctx);

        if self.trace && log::log_enabled!(log::Level::Trace) {
            let pc = self.regs.r[15].wrapping_sub(2);
            let s = self.disasm_thumb(instr, pc);
            let regs = self.dump_regs();
            trace!("{pc:08X}:     {instr:04X}: {s:24} {regs}",);
        }

        let ix = instr >> 6;
        self.thumb_op_table[ix as usize](self, ctx, instr);
    }

    fn fetch32(&mut self, ctx: &mut impl Context) -> u32 {
        let pc = self.regs.r[15];
        assert_eq!(pc & 0x3, 0, "Fetch from unaligned address: 0x{pc:08X}");

        self.regs.r[15] = self.regs.r[15].wrapping_add(4);
        let first = self.fetch_first;
        self.fetch_first = false;
        ctx.read32(pc, first)
    }

    fn fetch16(&mut self, ctx: &mut impl Context) -> u16 {
        let pc = self.regs.r[15];
        assert_eq!(pc & 0x1, 0, "Fetch from unaligned address: 0x{pc:08X}");

        self.regs.r[15] = self.regs.r[15].wrapping_add(2);
        let first = self.fetch_first;
        self.fetch_first = false;
        ctx.read16(pc, first)
    }
}

fn build_arm_table<C: Context>() -> ([ArmOp<C>; 0x1000], [ArmDisasm; 0x1000]) {
    let mut op_tbl = [arm_op_invalid as ArmOp<C>; 0x1000];
    let mut disasm_tbl = [arm_disasm_invalid as ArmDisasm; 0x1000];

    let invalid = |_hi: usize, _lo: usize| {
        // trace!(
        //     "Invalid code: CCCC {:04b} {:04b} .... .... .... {:04b} ....",
        //     hi >> 4,
        //     hi & 0xF,
        //     lo
        // );
        arm_op_invalid
    };

    for hi in 0..0x100 {
        macro_rules! ldsth {
            ($t:ty) => {
                match hi {
                    0b00000 => arm_op_ldsth::<C, $t, false, false, false, false, false>,
                    0b00001 => arm_op_ldsth::<C, $t, false, false, false, false, true>,
                    0b00010 => arm_op_ldsth::<C, $t, false, false, false, true, false>,
                    0b00011 => arm_op_ldsth::<C, $t, false, false, false, true, true>,
                    0b00100 => arm_op_ldsth::<C, $t, false, false, true, false, false>,
                    0b00101 => arm_op_ldsth::<C, $t, false, false, true, false, true>,
                    0b00110 => arm_op_ldsth::<C, $t, false, false, true, true, false>,
                    0b00111 => arm_op_ldsth::<C, $t, false, false, true, true, true>,
                    0b01000 => arm_op_ldsth::<C, $t, false, true, false, false, false>,
                    0b01001 => arm_op_ldsth::<C, $t, false, true, false, false, true>,
                    0b01010 => arm_op_ldsth::<C, $t, false, true, false, true, false>,
                    0b01011 => arm_op_ldsth::<C, $t, false, true, false, true, true>,
                    0b01100 => arm_op_ldsth::<C, $t, false, true, true, false, false>,
                    0b01101 => arm_op_ldsth::<C, $t, false, true, true, false, true>,
                    0b01110 => arm_op_ldsth::<C, $t, false, true, true, true, false>,
                    0b01111 => arm_op_ldsth::<C, $t, false, true, true, true, true>,
                    0b10000 => arm_op_ldsth::<C, $t, true, false, false, false, false>,
                    0b10001 => arm_op_ldsth::<C, $t, true, false, false, false, true>,
                    0b10010 => arm_op_ldsth::<C, $t, true, false, false, true, false>,
                    0b10011 => arm_op_ldsth::<C, $t, true, false, false, true, true>,
                    0b10100 => arm_op_ldsth::<C, $t, true, false, true, false, false>,
                    0b10101 => arm_op_ldsth::<C, $t, true, false, true, false, true>,
                    0b10110 => arm_op_ldsth::<C, $t, true, false, true, true, false>,
                    0b10111 => arm_op_ldsth::<C, $t, true, false, true, true, true>,
                    0b11000 => arm_op_ldsth::<C, $t, true, true, false, false, false>,
                    0b11001 => arm_op_ldsth::<C, $t, true, true, false, false, true>,
                    0b11010 => arm_op_ldsth::<C, $t, true, true, false, true, false>,
                    0b11011 => arm_op_ldsth::<C, $t, true, true, false, true, true>,
                    0b11100 => arm_op_ldsth::<C, $t, true, true, true, false, false>,
                    0b11101 => arm_op_ldsth::<C, $t, true, true, true, false, true>,
                    0b11110 => arm_op_ldsth::<C, $t, true, true, true, true, false>,
                    0b11111 => arm_op_ldsth::<C, $t, true, true, true, true, true>,
                    _ => unreachable!(),
                }
            };
        }

        macro_rules! alu {
            () => {
                match hi {
                    0b0_0000_0 => arm_op_alu::<C, false, false, 0>,
                    0b0_0000_1 => arm_op_alu::<C, false, true, 0>,
                    0b1_0000_0 => arm_op_alu::<C, true, false, 0>,
                    0b1_0000_1 => arm_op_alu::<C, true, true, 0>,

                    0b0_0001_0 => arm_op_alu::<C, false, false, 1>,
                    0b0_0001_1 => arm_op_alu::<C, false, true, 1>,
                    0b1_0001_0 => arm_op_alu::<C, true, false, 1>,
                    0b1_0001_1 => arm_op_alu::<C, true, true, 1>,

                    0b0_0010_0 => arm_op_alu::<C, false, false, 2>,
                    0b0_0010_1 => arm_op_alu::<C, false, true, 2>,
                    0b1_0010_0 => arm_op_alu::<C, true, false, 2>,
                    0b1_0010_1 => arm_op_alu::<C, true, true, 2>,

                    0b0_0011_0 => arm_op_alu::<C, false, false, 3>,
                    0b0_0011_1 => arm_op_alu::<C, false, true, 3>,
                    0b1_0011_0 => arm_op_alu::<C, true, false, 3>,
                    0b1_0011_1 => arm_op_alu::<C, true, true, 3>,

                    0b0_0100_0 => arm_op_alu::<C, false, false, 4>,
                    0b0_0100_1 => arm_op_alu::<C, false, true, 4>,
                    0b1_0100_0 => arm_op_alu::<C, true, false, 4>,
                    0b1_0100_1 => arm_op_alu::<C, true, true, 4>,

                    0b0_0101_0 => arm_op_alu::<C, false, false, 5>,
                    0b0_0101_1 => arm_op_alu::<C, false, true, 5>,
                    0b1_0101_0 => arm_op_alu::<C, true, false, 5>,
                    0b1_0101_1 => arm_op_alu::<C, true, true, 5>,

                    0b0_0110_0 => arm_op_alu::<C, false, false, 6>,
                    0b0_0110_1 => arm_op_alu::<C, false, true, 6>,
                    0b1_0110_0 => arm_op_alu::<C, true, false, 6>,
                    0b1_0110_1 => arm_op_alu::<C, true, true, 6>,

                    0b0_0111_0 => arm_op_alu::<C, false, false, 7>,
                    0b0_0111_1 => arm_op_alu::<C, false, true, 7>,
                    0b1_0111_0 => arm_op_alu::<C, true, false, 7>,
                    0b1_0111_1 => arm_op_alu::<C, true, true, 7>,

                    0b0_1000_0 => arm_op_alu::<C, false, false, 8>,
                    0b0_1000_1 => arm_op_alu::<C, false, true, 8>,
                    0b1_1000_0 => arm_op_alu::<C, true, false, 8>,
                    0b1_1000_1 => arm_op_alu::<C, true, true, 8>,

                    0b0_1001_0 => arm_op_alu::<C, false, false, 9>,
                    0b0_1001_1 => arm_op_alu::<C, false, true, 9>,
                    0b1_1001_0 => arm_op_alu::<C, true, false, 9>,
                    0b1_1001_1 => arm_op_alu::<C, true, true, 9>,

                    0b0_1010_0 => arm_op_alu::<C, false, false, 10>,
                    0b0_1010_1 => arm_op_alu::<C, false, true, 10>,
                    0b1_1010_0 => arm_op_alu::<C, true, false, 10>,
                    0b1_1010_1 => arm_op_alu::<C, true, true, 10>,

                    0b0_1011_0 => arm_op_alu::<C, false, false, 11>,
                    0b0_1011_1 => arm_op_alu::<C, false, true, 11>,
                    0b1_1011_0 => arm_op_alu::<C, true, false, 11>,
                    0b1_1011_1 => arm_op_alu::<C, true, true, 11>,

                    0b0_1100_0 => arm_op_alu::<C, false, false, 12>,
                    0b0_1100_1 => arm_op_alu::<C, false, true, 12>,
                    0b1_1100_0 => arm_op_alu::<C, true, false, 12>,
                    0b1_1100_1 => arm_op_alu::<C, true, true, 12>,

                    0b0_1101_0 => arm_op_alu::<C, false, false, 13>,
                    0b0_1101_1 => arm_op_alu::<C, false, true, 13>,
                    0b1_1101_0 => arm_op_alu::<C, true, false, 13>,
                    0b1_1101_1 => arm_op_alu::<C, true, true, 13>,

                    0b0_1110_0 => arm_op_alu::<C, false, false, 14>,
                    0b0_1110_1 => arm_op_alu::<C, false, true, 14>,
                    0b1_1110_0 => arm_op_alu::<C, true, false, 14>,
                    0b1_1110_1 => arm_op_alu::<C, true, true, 14>,

                    0b0_1111_0 => arm_op_alu::<C, false, false, 15>,
                    0b0_1111_1 => arm_op_alu::<C, false, true, 15>,
                    0b1_1111_0 => arm_op_alu::<C, true, false, 15>,
                    0b1_1111_1 => arm_op_alu::<C, true, true, 15>,

                    _ => unreachable!(),
                }
            };
        }

        for lo in 0..0x10 {
            let (op, disasm): (ArmOp<C>, ArmDisasm) = match hi >> 5 {
                0b000 => {
                    if hi == 0b00010010 && lo == 0b0001 {
                        (arm_op_bx, arm_disasm_bx)
                    } else if lo == 0b1001 {
                        match hi {
                            0b000_00 => (arm_op_mul::<C, false, false>, arm_disasm_mul),
                            0b000_01 => (arm_op_mul::<C, false, true>, arm_disasm_mul),
                            0b000_10 => (arm_op_mul::<C, true, false>, arm_disasm_mul),
                            0b000_11 => (arm_op_mul::<C, true, true>, arm_disasm_mul),

                            0b01_000 => (arm_op_mull::<C, false, false, false>, arm_disasm_mull),
                            0b01_001 => (arm_op_mull::<C, false, false, true>, arm_disasm_mull),
                            0b01_010 => (arm_op_mull::<C, false, true, false>, arm_disasm_mull),
                            0b01_011 => (arm_op_mull::<C, false, true, true>, arm_disasm_mull),
                            0b01_100 => (arm_op_mull::<C, true, false, false>, arm_disasm_mull),
                            0b01_101 => (arm_op_mull::<C, true, false, true>, arm_disasm_mull),
                            0b01_110 => (arm_op_mull::<C, true, true, false>, arm_disasm_mull),
                            0b01_111 => (arm_op_mull::<C, true, true, true>, arm_disasm_mull),

                            0b10_0_00 => (arm_op_swp::<C, false>, arm_disasm_swp),
                            0b10_1_00 => (arm_op_swp::<C, true>, arm_disasm_swp),

                            _ => (invalid(hi, lo), arm_disasm_invalid),
                        }
                    } else if lo == 0b1011 {
                        (ldsth!(u16), arm_disasm_ldsth)
                    } else if lo == 0b1101 {
                        (ldsth!(i8), arm_disasm_ldsth)
                    } else if lo == 0b1111 {
                        (ldsth!(i16), arm_disasm_ldsth)
                    } else if hi & 0b000_1100_1 == 0b000_1000_0 {
                        // compare with s = 0
                        if hi & 0b000_1101_1 == 0b000_1000_0 && lo == 0 {
                            let op = if hi & 0b000_1111_0 == 0b000_1000_0 {
                                arm_op_mrs::<C, false>
                            } else {
                                arm_op_mrs::<C, true>
                            };
                            (op, arm_disasm_mrs)
                        } else if hi & 0b000_1101_1 == 0b000_1001_0 {
                            let op = if hi & 0b000_1111_1 == 0b000_1001_0 {
                                arm_op_msr::<C, false, false>
                            } else {
                                arm_op_msr::<C, false, true>
                            };
                            (op, arm_disasm_msr)
                        } else {
                            (invalid(hi, lo), arm_disasm_invalid)
                        }
                    } else {
                        (alu!(), arm_disasm_alu)
                    }
                }

                0b001 => {
                    if hi & 0b000_1100_1 == 0b000_1000_0 {
                        // compare with s = 0
                        if hi & 0b000_1101_1 == 0b000_1001_0 {
                            let op = if hi & 0b000_1111_1 == 0b000_1001_0 {
                                arm_op_msr::<C, true, false>
                            } else {
                                arm_op_msr::<C, true, true>
                            };
                            (op, arm_disasm_msr)
                        } else {
                            (invalid(hi, lo), arm_disasm_invalid)
                        }
                    } else {
                        (alu!(), arm_disasm_alu)
                    }
                }

                0b011 if lo & 1 == 1 => (arm_op_undef, arm_disasm_undef),

                0b010 | 0b011 => {
                    let op = match hi {
                        0b01000000 => arm_op_ldst::<C, false, false, false, false, false, false>,
                        0b01000001 => arm_op_ldst::<C, false, false, false, false, false, true>,
                        0b01000010 => arm_op_ldst::<C, false, false, false, false, true, false>,
                        0b01000011 => arm_op_ldst::<C, false, false, false, false, true, true>,
                        0b01000100 => arm_op_ldst::<C, false, false, false, true, false, false>,
                        0b01000101 => arm_op_ldst::<C, false, false, false, true, false, true>,
                        0b01000110 => arm_op_ldst::<C, false, false, false, true, true, false>,
                        0b01000111 => arm_op_ldst::<C, false, false, false, true, true, true>,
                        0b01001000 => arm_op_ldst::<C, false, false, true, false, false, false>,
                        0b01001001 => arm_op_ldst::<C, false, false, true, false, false, true>,
                        0b01001010 => arm_op_ldst::<C, false, false, true, false, true, false>,
                        0b01001011 => arm_op_ldst::<C, false, false, true, false, true, true>,
                        0b01001100 => arm_op_ldst::<C, false, false, true, true, false, false>,
                        0b01001101 => arm_op_ldst::<C, false, false, true, true, false, true>,
                        0b01001110 => arm_op_ldst::<C, false, false, true, true, true, false>,
                        0b01001111 => arm_op_ldst::<C, false, false, true, true, true, true>,
                        0b01010000 => arm_op_ldst::<C, false, true, false, false, false, false>,
                        0b01010001 => arm_op_ldst::<C, false, true, false, false, false, true>,
                        0b01010010 => arm_op_ldst::<C, false, true, false, false, true, false>,
                        0b01010011 => arm_op_ldst::<C, false, true, false, false, true, true>,
                        0b01010100 => arm_op_ldst::<C, false, true, false, true, false, false>,
                        0b01010101 => arm_op_ldst::<C, false, true, false, true, false, true>,
                        0b01010110 => arm_op_ldst::<C, false, true, false, true, true, false>,
                        0b01010111 => arm_op_ldst::<C, false, true, false, true, true, true>,
                        0b01011000 => arm_op_ldst::<C, false, true, true, false, false, false>,
                        0b01011001 => arm_op_ldst::<C, false, true, true, false, false, true>,
                        0b01011010 => arm_op_ldst::<C, false, true, true, false, true, false>,
                        0b01011011 => arm_op_ldst::<C, false, true, true, false, true, true>,
                        0b01011100 => arm_op_ldst::<C, false, true, true, true, false, false>,
                        0b01011101 => arm_op_ldst::<C, false, true, true, true, false, true>,
                        0b01011110 => arm_op_ldst::<C, false, true, true, true, true, false>,
                        0b01011111 => arm_op_ldst::<C, false, true, true, true, true, true>,
                        0b01100000 => arm_op_ldst::<C, true, false, false, false, false, false>,
                        0b01100001 => arm_op_ldst::<C, true, false, false, false, false, true>,
                        0b01100010 => arm_op_ldst::<C, true, false, false, false, true, false>,
                        0b01100011 => arm_op_ldst::<C, true, false, false, false, true, true>,
                        0b01100100 => arm_op_ldst::<C, true, false, false, true, false, false>,
                        0b01100101 => arm_op_ldst::<C, true, false, false, true, false, true>,
                        0b01100110 => arm_op_ldst::<C, true, false, false, true, true, false>,
                        0b01100111 => arm_op_ldst::<C, true, false, false, true, true, true>,
                        0b01101000 => arm_op_ldst::<C, true, false, true, false, false, false>,
                        0b01101001 => arm_op_ldst::<C, true, false, true, false, false, true>,
                        0b01101010 => arm_op_ldst::<C, true, false, true, false, true, false>,
                        0b01101011 => arm_op_ldst::<C, true, false, true, false, true, true>,
                        0b01101100 => arm_op_ldst::<C, true, false, true, true, false, false>,
                        0b01101101 => arm_op_ldst::<C, true, false, true, true, false, true>,
                        0b01101110 => arm_op_ldst::<C, true, false, true, true, true, false>,
                        0b01101111 => arm_op_ldst::<C, true, false, true, true, true, true>,
                        0b01110000 => arm_op_ldst::<C, true, true, false, false, false, false>,
                        0b01110001 => arm_op_ldst::<C, true, true, false, false, false, true>,
                        0b01110010 => arm_op_ldst::<C, true, true, false, false, true, false>,
                        0b01110011 => arm_op_ldst::<C, true, true, false, false, true, true>,
                        0b01110100 => arm_op_ldst::<C, true, true, false, true, false, false>,
                        0b01110101 => arm_op_ldst::<C, true, true, false, true, false, true>,
                        0b01110110 => arm_op_ldst::<C, true, true, false, true, true, false>,
                        0b01110111 => arm_op_ldst::<C, true, true, false, true, true, true>,
                        0b01111000 => arm_op_ldst::<C, true, true, true, false, false, false>,
                        0b01111001 => arm_op_ldst::<C, true, true, true, false, false, true>,
                        0b01111010 => arm_op_ldst::<C, true, true, true, false, true, false>,
                        0b01111011 => arm_op_ldst::<C, true, true, true, false, true, true>,
                        0b01111100 => arm_op_ldst::<C, true, true, true, true, false, false>,
                        0b01111101 => arm_op_ldst::<C, true, true, true, true, false, true>,
                        0b01111110 => arm_op_ldst::<C, true, true, true, true, true, false>,
                        0b01111111 => arm_op_ldst::<C, true, true, true, true, true, true>,
                        _ => unreachable!(),
                    };
                    (op, arm_disasm_ldst)
                }

                0b100 => {
                    let op = match hi {
                        0b100_00000 => arm_op_ldstm::<C, false, false, false, false, false>,
                        0b100_00001 => arm_op_ldstm::<C, false, false, false, false, true>,
                        0b100_00010 => arm_op_ldstm::<C, false, false, false, true, false>,
                        0b100_00011 => arm_op_ldstm::<C, false, false, false, true, true>,
                        0b100_00100 => arm_op_ldstm::<C, false, false, true, false, false>,
                        0b100_00101 => arm_op_ldstm::<C, false, false, true, false, true>,
                        0b100_00110 => arm_op_ldstm::<C, false, false, true, true, false>,
                        0b100_00111 => arm_op_ldstm::<C, false, false, true, true, true>,
                        0b100_01000 => arm_op_ldstm::<C, false, true, false, false, false>,
                        0b100_01001 => arm_op_ldstm::<C, false, true, false, false, true>,
                        0b100_01010 => arm_op_ldstm::<C, false, true, false, true, false>,
                        0b100_01011 => arm_op_ldstm::<C, false, true, false, true, true>,
                        0b100_01100 => arm_op_ldstm::<C, false, true, true, false, false>,
                        0b100_01101 => arm_op_ldstm::<C, false, true, true, false, true>,
                        0b100_01110 => arm_op_ldstm::<C, false, true, true, true, false>,
                        0b100_01111 => arm_op_ldstm::<C, false, true, true, true, true>,
                        0b100_10000 => arm_op_ldstm::<C, true, false, false, false, false>,
                        0b100_10001 => arm_op_ldstm::<C, true, false, false, false, true>,
                        0b100_10010 => arm_op_ldstm::<C, true, false, false, true, false>,
                        0b100_10011 => arm_op_ldstm::<C, true, false, false, true, true>,
                        0b100_10100 => arm_op_ldstm::<C, true, false, true, false, false>,
                        0b100_10101 => arm_op_ldstm::<C, true, false, true, false, true>,
                        0b100_10110 => arm_op_ldstm::<C, true, false, true, true, false>,
                        0b100_10111 => arm_op_ldstm::<C, true, false, true, true, true>,
                        0b100_11000 => arm_op_ldstm::<C, true, true, false, false, false>,
                        0b100_11001 => arm_op_ldstm::<C, true, true, false, false, true>,
                        0b100_11010 => arm_op_ldstm::<C, true, true, false, true, false>,
                        0b100_11011 => arm_op_ldstm::<C, true, true, false, true, true>,
                        0b100_11100 => arm_op_ldstm::<C, true, true, true, false, false>,
                        0b100_11101 => arm_op_ldstm::<C, true, true, true, false, true>,
                        0b100_11110 => arm_op_ldstm::<C, true, true, true, true, false>,
                        0b100_11111 => arm_op_ldstm::<C, true, true, true, true, true>,
                        _ => unreachable!(),
                    };
                    (op, arm_disasm_ldstm)
                }

                0b101 => {
                    let op = if (hi >> 4) & 1 == 0 {
                        arm_op_b::<C, false>
                    } else {
                        arm_op_b::<C, true>
                    };
                    (op, arm_disasm_b)
                }

                0b110 => {
                    let op = match hi {
                        0b110_00000 => arm_op_ldstc::<C, false, false, false, false, false>,
                        0b110_00001 => arm_op_ldstc::<C, false, false, false, false, true>,
                        0b110_00010 => arm_op_ldstc::<C, false, false, false, true, false>,
                        0b110_00011 => arm_op_ldstc::<C, false, false, false, true, true>,
                        0b110_00100 => arm_op_ldstc::<C, false, false, true, false, false>,
                        0b110_00101 => arm_op_ldstc::<C, false, false, true, false, true>,
                        0b110_00110 => arm_op_ldstc::<C, false, false, true, true, false>,
                        0b110_00111 => arm_op_ldstc::<C, false, false, true, true, true>,
                        0b110_01000 => arm_op_ldstc::<C, false, true, false, false, false>,
                        0b110_01001 => arm_op_ldstc::<C, false, true, false, false, true>,
                        0b110_01010 => arm_op_ldstc::<C, false, true, false, true, false>,
                        0b110_01011 => arm_op_ldstc::<C, false, true, false, true, true>,
                        0b110_01100 => arm_op_ldstc::<C, false, true, true, false, false>,
                        0b110_01101 => arm_op_ldstc::<C, false, true, true, false, true>,
                        0b110_01110 => arm_op_ldstc::<C, false, true, true, true, false>,
                        0b110_01111 => arm_op_ldstc::<C, false, true, true, true, true>,
                        0b110_10000 => arm_op_ldstc::<C, true, false, false, false, false>,
                        0b110_10001 => arm_op_ldstc::<C, true, false, false, false, true>,
                        0b110_10010 => arm_op_ldstc::<C, true, false, false, true, false>,
                        0b110_10011 => arm_op_ldstc::<C, true, false, false, true, true>,
                        0b110_10100 => arm_op_ldstc::<C, true, false, true, false, false>,
                        0b110_10101 => arm_op_ldstc::<C, true, false, true, false, true>,
                        0b110_10110 => arm_op_ldstc::<C, true, false, true, true, false>,
                        0b110_10111 => arm_op_ldstc::<C, true, false, true, true, true>,
                        0b110_11000 => arm_op_ldstc::<C, true, true, false, false, false>,
                        0b110_11001 => arm_op_ldstc::<C, true, true, false, false, true>,
                        0b110_11010 => arm_op_ldstc::<C, true, true, false, true, false>,
                        0b110_11011 => arm_op_ldstc::<C, true, true, false, true, true>,
                        0b110_11100 => arm_op_ldstc::<C, true, true, true, false, false>,
                        0b110_11101 => arm_op_ldstc::<C, true, true, true, false, true>,
                        0b110_11110 => arm_op_ldstc::<C, true, true, true, true, false>,
                        0b110_11111 => arm_op_ldstc::<C, true, true, true, true, true>,
                        _ => unreachable!(),
                    };
                    (op, arm_disasm_ldstc)
                }

                0b111 => {
                    if hi & 0xF0 == 0xF0 {
                        (arm_op_swi, arm_disasm_swi)
                    } else {
                        if lo & 1 == 0 {
                            (arm_op_cdp, arm_disasm_cdp)
                        } else {
                            if hi & 1 == 0 {
                                (arm_op_mrc, arm_disasm_mrc_mcr)
                            } else {
                                (arm_op_mcr, arm_disasm_mrc_mcr)
                            }
                        }
                    }
                }
                _ => unreachable!(),
            };

            op_tbl[(hi << 4) | lo] = op;
            disasm_tbl[(hi << 4) | lo] = disasm;
        }
    }

    (op_tbl, disasm_tbl)
}

fn build_thumb_table<C: Context>() -> ([ThumbOp<C>; 0x400], [ThumbDisasm; 0x400]) {
    let mut op_tbl = [thumb_op_invalid as ThumbOp<C>; 0x400];
    let mut disasm_tbl = [thumb_disasm_invalid as ThumbDisasm; 0x400];

    let invalid = |_ix: usize| -> ThumbOp<C> {
        // trace!(
        //     "Invalid thumb instruction {:04b} {:04b} {:02b}.. ....",
        //     (ix >> 6),
        //     (ix >> 2) & 0b1111,
        //     ix & 0b11
        // );
        thumb_op_invalid
    };

    for ix in 0..0x400 {
        let (op, disasm): (ThumbOp<C>, ThumbDisasm) = if (ix >> 7) == 0b000 {
            match (ix >> 5) & 3 {
                0 => (thumb_op_shift::<C, 0>, thumb_disasm_shift),
                1 => (thumb_op_shift::<C, 1>, thumb_disasm_shift),
                2 => (thumb_op_shift::<C, 2>, thumb_disasm_shift),
                3 => match (ix >> 3) & 3 {
                    0b00 => (thumb_op_add_sub::<C, false, false>, thumb_disasm_add_sub),
                    0b01 => (thumb_op_add_sub::<C, false, true>, thumb_disasm_add_sub),
                    0b10 => (thumb_op_add_sub::<C, true, false>, thumb_disasm_add_sub),
                    0b11 => (thumb_op_add_sub::<C, true, true>, thumb_disasm_add_sub),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        } else if (ix >> 7) == 0b001 {
            let op = match (ix >> 5) & 3 {
                0 => thumb_op_alu_imm8::<C, 0>,
                1 => thumb_op_alu_imm8::<C, 1>,
                2 => thumb_op_alu_imm8::<C, 2>,
                3 => thumb_op_alu_imm8::<C, 3>,
                _ => unreachable!(),
            };
            (op, thumb_disasm_alu_imm8)
        } else if (ix >> 4) == 0b010000 {
            let op = match ix & 0b1111 {
                0 => thumb_op_alu::<C, 0>,
                1 => thumb_op_alu::<C, 1>,
                2 => thumb_op_alu::<C, 2>,
                3 => thumb_op_alu::<C, 3>,
                4 => thumb_op_alu::<C, 4>,
                5 => thumb_op_alu::<C, 5>,
                6 => thumb_op_alu::<C, 6>,
                7 => thumb_op_alu::<C, 7>,
                8 => thumb_op_alu::<C, 8>,
                9 => thumb_op_alu::<C, 9>,
                10 => thumb_op_alu::<C, 10>,
                11 => thumb_op_alu::<C, 11>,
                12 => thumb_op_alu::<C, 12>,
                13 => thumb_op_alu::<C, 13>,
                14 => thumb_op_alu::<C, 14>,
                15 => thumb_op_alu::<C, 15>,
                _ => unreachable!(),
            };
            (op, thumb_disasm_alu)
        } else if (ix >> 4) == 0b010001 {
            let op = match ix & 0b1111 {
                0b0001 => thumb_op_hireg::<C, 0, false, true>,
                0b0010 => thumb_op_hireg::<C, 0, true, false>,
                0b0011 => thumb_op_hireg::<C, 0, true, true>,

                0b0101 => thumb_op_hireg::<C, 1, false, true>,
                0b0110 => thumb_op_hireg::<C, 1, true, false>,
                0b0111 => thumb_op_hireg::<C, 1, true, true>,

                0b1001 => thumb_op_hireg::<C, 2, false, true>,
                0b1010 => thumb_op_hireg::<C, 2, true, false>,
                0b1011 => thumb_op_hireg::<C, 2, true, true>,

                0b1100 => thumb_op_bx::<C, false>,
                0b1101 => thumb_op_bx::<C, true>,

                _ => invalid(ix),
            };
            (op, thumb_disasm_hireg)
        } else if (ix >> 5) == 0b01001 {
            (thumb_op_ld_pcrel, thumb_disasm_ld_pcrel)
        } else if (ix >> 6) == 0b0101 {
            let op = match (ix >> 3) & 0b111 {
                0b000 => thumb_op_ldst::<C, false, u32>,
                0b010 => thumb_op_ldst::<C, false, u8>,
                0b100 => thumb_op_ldst::<C, true, u32>,
                0b110 => thumb_op_ldst::<C, true, u8>,
                0b001 => thumb_op_ldst::<C, false, u16>,
                0b011 => thumb_op_ldst::<C, true, i8>,
                0b101 => thumb_op_ldst::<C, true, u16>,
                0b111 => thumb_op_ldst::<C, true, i16>,
                _ => unreachable!(),
            };
            (op, thumb_disasm_ldst)
        } else if (ix >> 7) == 0b011 {
            let op = match (ix >> 5) & 3 {
                0b00 => thumb_op_ldst_ofs::<C, false, u32>,
                0b01 => thumb_op_ldst_ofs::<C, true, u32>,
                0b10 => thumb_op_ldst_ofs::<C, false, u8>,
                0b11 => thumb_op_ldst_ofs::<C, true, u8>,
                _ => unreachable!(),
            };
            (op, thumb_disasm_ldst_ofs)
        } else if (ix >> 6) == 0b1000 {
            let op = if (ix >> 5) & 1 == 0 {
                thumb_op_ldst_ofs::<C, false, u16>
            } else {
                thumb_op_ldst_ofs::<C, true, u16>
            };
            (op, thumb_disasm_ldst_ofs)
        } else if (ix >> 6) == 0b1001 {
            let op = if (ix >> 5) & 1 == 0 {
                thumb_op_ldst_sprel::<C, false>
            } else {
                thumb_op_ldst_sprel::<C, true>
            };
            (op, thumb_disasm_ldst_sprel)
        } else if (ix >> 6) == 0b1010 {
            let op = if (ix >> 5) & 1 == 0 {
                thumb_op_load_addr::<C, false>
            } else {
                thumb_op_load_addr::<C, true>
            };
            (op, thumb_disasm_load_addr)
        } else if (ix >> 2) == 0b10110000 {
            let op = if (ix >> 1) & 1 == 0 {
                thumb_op_add_sp::<C, false>
            } else {
                thumb_op_add_sp::<C, true>
            };
            (op, thumb_disasm_add_sp)
        } else if (ix >> 2) & 0b11110110 == 0b10110100 {
            let op = match (ix >> 2) & 0b1001 {
                0b0000 => thumb_op_push_pop::<C, false, false>,
                0b0001 => thumb_op_push_pop::<C, false, true>,
                0b1000 => thumb_op_push_pop::<C, true, false>,
                0b1001 => thumb_op_push_pop::<C, true, true>,
                _ => unreachable!(),
            };
            (op, thumb_disasm_push_pop)
        } else if (ix >> 6) == 0b1100 {
            let op = if (ix >> 5) & 1 == 0 {
                thumb_op_ldstm::<C, false>
            } else {
                thumb_op_ldstm::<C, true>
            };
            (op, thumb_disasm_ldstm)
        } else if (ix >> 6) == 0b1101 {
            match (ix >> 2) & 15 {
                0 => (thumb_op_cond_branch::<C, 0>, thumb_disasm_cond_branch),
                1 => (thumb_op_cond_branch::<C, 1>, thumb_disasm_cond_branch),
                2 => (thumb_op_cond_branch::<C, 2>, thumb_disasm_cond_branch),
                3 => (thumb_op_cond_branch::<C, 3>, thumb_disasm_cond_branch),
                4 => (thumb_op_cond_branch::<C, 4>, thumb_disasm_cond_branch),
                5 => (thumb_op_cond_branch::<C, 5>, thumb_disasm_cond_branch),
                6 => (thumb_op_cond_branch::<C, 6>, thumb_disasm_cond_branch),
                7 => (thumb_op_cond_branch::<C, 7>, thumb_disasm_cond_branch),
                8 => (thumb_op_cond_branch::<C, 8>, thumb_disasm_cond_branch),
                9 => (thumb_op_cond_branch::<C, 9>, thumb_disasm_cond_branch),
                10 => (thumb_op_cond_branch::<C, 10>, thumb_disasm_cond_branch),
                11 => (thumb_op_cond_branch::<C, 11>, thumb_disasm_cond_branch),
                12 => (thumb_op_cond_branch::<C, 12>, thumb_disasm_cond_branch),
                13 => (thumb_op_cond_branch::<C, 13>, thumb_disasm_cond_branch),
                14 => (thumb_op_cond_branch::<C, 14>, thumb_disasm_cond_branch),
                15 => (thumb_op_swi, thumb_disasm_swi),
                _ => unreachable!(),
            }
        } else if (ix >> 5) == 0b11100 {
            (thumb_op_b, thumb_disasm_b)
        } else if (ix >> 6) == 0b1111 {
            let op = if (ix >> 5) & 1 == 0 {
                thumb_op_bl::<C, false>
            } else {
                thumb_op_bl::<C, true>
            };
            (op, thumb_disasm_bl)
        } else {
            (invalid(ix), thumb_disasm_invalid)
        };

        op_tbl[ix] = op;
        disasm_tbl[ix] = disasm;
    }

    (op_tbl, disasm_tbl)
}

#[rustfmt::skip]
const COND: [&str; 15] = [
    "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
    "hi", "ls", "ge", "lt", "gt", "le", "",
];

fn disasm_cond(instr: u32) -> &'static str {
    COND[(instr >> 28) as usize]
}

fn arm_op_bx<C: Context>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    // TODO: 2S + 1N cycles
    assert_eq!(instr & 0x0ffffff0, 0x012fff10);
    let rn = (instr & 0xF) as usize;
    let new_pc = cpu.regs.r[rn];
    cpu.regs.state = new_pc & 1 != 0;
    cpu.set_pc(new_pc & !1);
}

fn arm_disasm_bx(instr: u32, _pc: u32) -> String {
    // cccc 0001 0010 1111 1111 1111 0001 nnnn
    assert_eq!(instr & 0x0FFFFFF0, 0x012FFF10);
    let cond = disasm_cond(instr);
    let rn = instr & 0xF;
    format!("bx{cond} r{rn}")
}

fn arm_op_b<C: Context, const L: bool>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    // TODO: 2S + 1N cycles
    let offset = (((instr & 0xFFFFFF) << 8) as i32 >> 8) << 2;
    let old_pc = cpu.regs.r[15];
    if L {
        cpu.regs.r[14] = old_pc.wrapping_sub(4);
    }
    cpu.set_pc(old_pc.wrapping_add(offset as u32));
}

fn arm_disasm_b(instr: u32, pc: u32) -> String {
    let cond = disasm_cond(instr);
    let ofs = ((((instr & 0xFFFFFF) << 8) as i32 >> 8) << 2) as u32;
    let dest = pc.wrapping_add(8).wrapping_add(ofs);
    let l = if (instr & 0x01000000) != 0 { "l" } else { "" };
    format!("b{l}{cond} 0x{dest:08X}")
}

fn decode_reg_sft_imm(cpu: &mut Cpu<impl Context>, instr: u32) -> (u32, bool) {
    assert!(instr & 0x10 == 0);

    let rm = cpu.regs.r[(instr & 0xF) as usize];
    let shift_type = (instr >> 5) & 3;
    let amount = (instr >> 7) & 0x1F;

    if shift_type == 0 && amount == 0 {
        // LSL #0 is a special case, where the shifter carry out is the old value of the CPSR C flag.
        // The contents of Rm are used directly as the second operand

        (rm, cpu.regs.c_flag)
    } else if shift_type == 3 && amount == 0 {
        // The form of the shift field which might be expected to give ROR #0 is used to encode a special function of the barrel shifter, rotate right extended (RRX). This is a rotate right by one bit position of the 33 bit quantity formed by appending the CPSR C flag to the most significant end of the contents of Rm as shown in ➲Figure 4-10: Rotate right extended.
        let old_c = cpu.regs.c_flag as u32;
        ((rm >> 1) | (old_c << 31), rm & 1 != 0)
    } else {
        // The form of the shift field which might be expected to correspond to LSR #0 is used to encode LSR #32, which has a zero result with bit 31 of Rm as the carry output.
        // The form of the shift field which might be expected to give ASR #0 is used to encode ASR #32. Bit 31 of Rm is again used as the carry output, and each bit of operand 2 is also equal to bit 31 of Rm. The result is therefore all ones or all zeros, according to the value of bit 31 of Rm
        calc_sft(shift_type, rm, if amount == 0 { 32 } else { amount as u8 })
    }
}

fn decode_reg_sft_reg(cpu: &mut Cpu<impl Context>, instr: u32) -> (u32, bool) {
    assert!(instr & 0x10 != 0);

    let rm = (instr & 0xF) as usize;

    // If a register is used to specify the shift amount the PC will be 12 bytes ahead
    let rm = if rm != 15 {
        cpu.regs.r[rm]
    } else {
        cpu.regs.r[rm].wrapping_add(4)
    };

    let shift_type = (instr >> 5) & 0x3;

    let rs = ((instr >> 8) & 0xF) as usize;
    // Rs can be any general register other than R15
    assert_ne!(rs, 15);
    // Only the least significant byte of the contents of Rs is used to determine the shift amount.
    let amount = cpu.regs.r[rs] as u8;

    // If this byte is zero, the unchanged contents of Rm will be used as the second operand, and the old value of the CPSR C flag will be passed on as the shifter carry output.

    if amount == 0 {
        (rm, cpu.regs.c_flag)
    } else {
        calc_sft(shift_type, rm, amount)
    }
}

fn calc_sft(shift_type: u32, a: u32, b: u8) -> (u32, bool) {
    assert_ne!(b, 0);
    match shift_type {
        // LSL
        0 => {
            if b < 32 {
                (a << b, (a >> (32 - b)) & 1 != 0)
            } else if b == 32 {
                (0, a & 1 != 0)
            } else {
                (0, false)
            }
        }
        // LSR
        1 => {
            if b < 32 {
                (a >> b, (a >> (b - 1)) & 1 != 0)
            } else if b == 32 {
                (0, (a >> 31) & 1 != 0)
            } else {
                (0, false)
            }
        }
        // ASR
        2 => {
            if b < 32 {
                (((a as i32) >> b) as u32, (a >> (b - 1)) & 1 != 0)
            } else if (a >> 31) & 1 == 0 {
                (0, false)
            } else {
                (!0, true)
            }
        }
        // ROR
        3 => {
            let b = b & 0x1F;
            let data = a.rotate_right((b & 0x1F) as u32);
            let carry = if b > 0 {
                (a >> (b - 1)) & 1 != 0
            } else {
                (a >> 31) & 1 != 0
            };
            (data, carry)
        }
        _ => unreachable!(),
    }
}

fn decode_op2<C: Context, const I: bool>(cpu: &mut Cpu<C>, instr: u32) -> (u32, bool, bool) {
    if !I {
        if instr & 0x10 == 0 {
            let (ret, c_flag) = decode_reg_sft_imm(cpu, instr);
            (ret, c_flag, false)
        } else {
            let (ret, c_flag) = decode_reg_sft_reg(cpu, instr);
            (ret, c_flag, true)
        }
    } else {
        let imm = instr & 0xFF;
        let rot = (instr >> 8) & 0xF;
        let ret = imm.rotate_right(rot * 2);
        let c_flag = if rot != 0 {
            ret >> 31 != 0
        } else {
            cpu.regs.c_flag
        };
        (ret, c_flag, false)
    }
}

fn add_with_flag(a: u32, b: u32, c: bool) -> (u32, bool, bool) {
    let (ret, carry1) = a.overflowing_add(b);
    let (ret, carry2) = ret.overflowing_add(c as u32);

    let (tmp, overflow1) = (a as i32).overflowing_add(b as i32);
    let (_, overflow2) = tmp.overflowing_add(c as i32);

    (ret, carry1 || carry2, overflow1 || overflow2)
}

fn sub_with_flag(a: u32, b: u32, c: bool) -> (u32, bool, bool) {
    let (ret, carry1) = a.overflowing_sub(b);
    let (ret, carry2) = ret.overflowing_sub(1 - c as u32);

    let (tmp, overflow1) = (a as i32).overflowing_sub(b as i32);
    let (_, overflow2) = tmp.overflowing_sub(1 - c as i32);

    (ret, !(carry1 || carry2), overflow1 || overflow2)
}

fn alu<C: Context, const O: u8>(
    cpu: &mut Cpu<C>,
    op1: u32,
    op2: u32,
    c_flag: bool,
    change_flag: bool,
) -> Option<u32> {
    let (ret, cmp) = match O {
        // AND
        0b0000 => (op1 & op2, false),
        // EOR
        0b0001 => (op1 ^ op2, false),
        // SUB
        0b0010 => {
            if !change_flag {
                (op1.wrapping_sub(op2), false)
            } else {
                let (ret, carry, overflow) = sub_with_flag(op1, op2, true);
                cpu.regs.c_flag = carry;
                cpu.regs.v_flag = overflow;
                (ret, false)
            }
        }
        // RSB
        0b0011 => {
            if !change_flag {
                (op2.wrapping_sub(op1), false)
            } else {
                let (ret, carry, overflow) = sub_with_flag(op2, op1, true);
                cpu.regs.c_flag = carry;
                cpu.regs.v_flag = overflow;
                (ret, false)
            }
        }
        // ADD
        0b0100 => {
            if !change_flag {
                (op1.wrapping_add(op2), false)
            } else {
                let (ret, carry, overflow) = add_with_flag(op1, op2, false);
                cpu.regs.c_flag = carry;
                cpu.regs.v_flag = overflow;
                (ret, false)
            }
        }
        // ADC
        0b0101 => {
            if !change_flag {
                (op1.wrapping_add(op2).wrapping_add(c_flag as u32), false)
            } else {
                let (ret, carry, overflow) = add_with_flag(op1, op2, c_flag);
                cpu.regs.c_flag = carry;
                cpu.regs.v_flag = overflow;
                (ret, false)
            }
        }
        // SBC
        0b0110 => {
            if !change_flag {
                let borrow = 1 - cpu.regs.c_flag as u32;
                (op1.wrapping_sub(op2).wrapping_sub(borrow), false)
            } else {
                let (ret, carry, overflow) = sub_with_flag(op1, op2, c_flag);
                cpu.regs.c_flag = carry;
                cpu.regs.v_flag = overflow;
                (ret, false)
            }
        }
        // RSC
        0b0111 => {
            if !change_flag {
                let borrow = 1 - cpu.regs.c_flag as u32;
                (op2.wrapping_sub(op1).wrapping_sub(borrow), false)
            } else {
                let (ret, carry, overflow) = sub_with_flag(op2, op1, c_flag);
                cpu.regs.c_flag = carry;
                cpu.regs.v_flag = overflow;
                (ret, false)
            }
        }
        // TST
        0b1000 => (op1 & op2, true),
        // TEQ
        0b1001 => (op1 ^ op2, true),
        // CMP
        0b1010 => {
            let (ret, carry, overflow) = sub_with_flag(op1, op2, true);
            cpu.regs.c_flag = carry;
            cpu.regs.v_flag = overflow;
            (ret, true)
        }
        // CMN
        0b1011 => {
            let (ret, carry, overflow) = add_with_flag(op1, op2, false);
            cpu.regs.c_flag = carry;
            cpu.regs.v_flag = overflow;
            (ret, true)
        }
        // ORR
        0b1100 => (op1 | op2, false),
        // MOV
        0b1101 => (op2, false),
        // BIC
        0b1110 => (op1 & !op2, false),
        // MVN
        0b1111 => (!op2, false),

        _ => unreachable!(),
    };

    if change_flag {
        cpu.regs.set_nz(ret);
    }

    if !cmp {
        Some(ret)
    } else {
        None
    }
}

fn arm_op_alu<C: Context, const I: bool, const S: bool, const O: u8>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u32,
) {
    let rn = ((instr >> 16) & 0xF) as usize;
    let rd = ((instr >> 12) & 0xF) as usize;

    let change_flag = S && rd != 15;

    let c_flag = cpu.regs.c_flag;
    let (op2, carry, shift_by_reg) = decode_op2::<C, I>(cpu, instr);
    if change_flag {
        cpu.regs.c_flag = carry;
    }

    let op1 = if !(rn == 15 && shift_by_reg) {
        cpu.regs.r[rn]
    } else {
        cpu.regs.r[rn].wrapping_add(4)
    };

    if let Some(res) = alu::<C, O>(cpu, op1, op2, c_flag, change_flag) {
        if rd != 15 {
            cpu.regs.r[rd] = res;
        } else {
            cpu.set_pc(res);
            if S {
                assert!(cpu.regs.mode != MODE_USER);
                cpu.regs.set_cpsr(cpu.regs.spsr);
            }
        }
    } else if rd == 15 {
        assert!(cpu.regs.mode != MODE_USER);
        cpu.regs.set_cpsr(cpu.regs.spsr);
    }
}

fn disasm_op2(instr: u32) -> Option<String> {
    let i = (instr >> 25) & 1;

    if i != 0 {
        let imm = instr & 0xFF;
        let rot = (instr >> 8) & 0xF;
        let opr = imm.rotate_right(rot * 2);
        return if opr < 10 {
            Some(format!("#{opr}"))
        } else {
            Some(format!("#0x{opr:x}"))
        };
    }

    let rm = instr & 0xF;
    let shift_by_reg = (instr >> 4) & 1 != 0;
    let ty = (instr >> 5) & 3;

    const SHIFT_TYPE: [&str; 4] = ["lsl", "lsr", "asr", "ror"];
    let ty = SHIFT_TYPE[ty as usize];

    if shift_by_reg {
        if (instr >> 7) & 1 != 0 {
            return None;
        }
        let rs = (instr >> 8) & 0xF;
        return Some(format!("r{rm}, {ty} r{rs}"));
    }

    let amo = (instr >> 7) & 0x1F;
    if amo == 0 && ty == "lsl" {
        Some(format!("r{rm}"))
    } else if amo == 0 && ty == "ror" {
        Some(format!("r{rm}, rrx"))
    } else {
        Some(format!("r{rm}, {ty} #{amo}"))
    }
}

fn arm_disasm_alu(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let opc = (instr >> 21) & 0xF;
    let s = (instr >> 20) & 1;

    #[rustfmt::skip]
    const MNE: [&str; 16] = [
        "and", "eor", "sub", "rsb", "add", "adc", "sbc", "rsc",
        "tst", "teq", "cmp", "cmn", "orr", "mov", "bic", "mvn",
    ];

    let mne = MNE[opc as usize];
    let s = if s != 0 { "s" } else { "" };
    let op2 = disasm_op2(instr).unwrap();
    let rn = (instr >> 16) & 0xF;
    let rd = (instr >> 12) & 0xF;

    match mne {
        "mov" | "mvn" => format!("{mne}{cond}{s} r{rd}, {op2}"),
        "cmp" | "cmn" | "teq" | "tst" => format!("{mne} r{rn}, {op2}"),
        _ => format!("{mne}{cond}{s} r{rd}, r{rn}, {op2}"),
    }
}

fn arm_op_mrs<C: Context, const S: bool>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    assert_eq!(instr & 0x0FBF0FFF, 0x010F0000);
    let rd = ((instr >> 12) & 0xF) as usize;
    assert_ne!(rd, 15);
    if !S {
        cpu.regs.r[rd] = cpu.regs.cpsr();
    } else {
        cpu.regs.r[rd] = cpu.regs.spsr;
    }
}

fn arm_disasm_mrs(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let p = (instr >> 22) & 1;
    let psr = if p == 0 { "cpsr" } else { "spsr" };
    let rd = (instr >> 12) & 0xF;

    if instr & 0x0FBF0FFF == 0x010F0000 {
        // cccc 0001 0p00 1111 dddd 0000 0000 0000
        format!("mrs{cond} r{rd}, {psr}")
    } else {
        panic!()
    }
}

fn arm_op_msr<C: Context, const I: bool, const S: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u32,
) {
    if !I {
        assert!(instr & 0x0FB6FFF0 == 0x0120F000, "instr: {instr:032b}");
    } else {
        assert!(instr & 0x0FB6F000 == 0x0320F000, "instr: {instr:032b}");
    }

    let f = (instr >> 19) & 1 != 0;
    let c = (instr >> 16) & 1 != 0;

    let src = decode_op2::<C, I>(cpu, instr).0;

    let subst = |psr: u32| -> u32 {
        let mut ret = psr;
        if f {
            ret = ret & !0xF0000000 | src & 0xF0000000;
        }
        if c {
            ret = ret & !0x000000FF | src & 0x000000FF;
        }
        ret
    };

    if S {
        cpu.regs.spsr = subst(cpu.regs.spsr);
    } else {
        cpu.regs.set_cpsr(subst(cpu.regs.cpsr()));
    }
}

fn arm_disasm_msr(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let p = (instr >> 22) & 1;
    let psr = if p == 0 { "cpsr" } else { "spsr" };
    let f = if (instr >> 19) & 1 != 0 { "f" } else { "" };
    let c = if (instr >> 16) & 1 != 0 { "c" } else { "" };
    let rm = instr & 0xF;
    if instr & 0x0FBFFFF0 == 0x0129F000 {
        // cccc 0001 0p10 f00c 1111 0000 0000 mmmm
        format!("msr{cond} {psr}_{c}{f}, r{rm}")
    } else if instr & 0x0FB6F000 == 0x0320F000 {
        // cccc 0011 0p10 f00c 1111 rrrr iiii iiii
        let rot = (instr >> 8) & 0xF;
        let imm = (instr & 0xFF).rotate_right(rot * 2);
        format!("msr{cond} {psr}_{c}{f}, #0x{imm:x}")
    } else {
        panic!("invalid instr: {instr:08X}")
    }
}

fn arm_op_mul<C: Context, const A: bool, const S: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u32,
) {
    // TODO: cycles

    let rd = ((instr >> 16) & 0xF) as usize;
    let rn = ((instr >> 12) & 0xF) as usize;
    let rs = ((instr >> 8) & 0xF) as usize;
    let rm = (instr & 0xF) as usize;

    // The destination register Rd must not be the same as the operand register Rm.
    // R15 must not be used as an operand or as the destination register.

    // Is this OK?
    // assert_ne!(rd, rm);

    assert_ne!(rd, 15);
    assert_ne!(rn, 15);
    assert_ne!(rs, 15);
    assert_ne!(rm, 15);

    let rm = cpu.regs.r[rm];
    let rs = cpu.regs.r[rs];

    let res = if !A {
        rm.wrapping_mul(rs)
    } else {
        let rn = cpu.regs.r[rn];
        rm.wrapping_mul(rs).wrapping_add(rn)
    };

    cpu.regs.r[rd] = res;
    if S {
        cpu.regs.set_nz(res);
    }
}

fn arm_disasm_mul(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let rd = (instr >> 16) & 0xF;
    let rn = (instr >> 12) & 0xF;
    let rs = (instr >> 8) & 0xF;
    let rm = instr & 0xF;
    let a = (instr >> 21) & 1;
    let s = (instr >> 20) & 1;
    let s = if s == 0 { "" } else { "s" };

    if a == 0 {
        format!("mul{cond}{s} r{rd}, r{rm}, r{rs}")
    } else {
        format!("mla{cond}{s} r{rd}, r{rm}, r{rs}, r{rn}")
    }
}

fn arm_op_mull<C: Context, const U: bool, const A: bool, const S: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u32,
) {
    let rdhi = ((instr >> 16) & 0xF) as usize;
    let rdlo = ((instr >> 12) & 0xF) as usize;
    let rs = ((instr >> 8) & 0xF) as usize;
    let rm = (instr & 0xF) as usize;

    // R15 must not be used as an operand or as a destination register.
    // RdHi, RdLo, and Rm must all specify different registers.

    assert_ne!(rdhi, 15);
    assert_ne!(rdlo, 15);
    assert_ne!(rs, 15);
    assert_ne!(rm, 15);
    assert_ne!(rdhi, rdlo);
    assert_ne!(rdhi, rm);
    assert_ne!(rdlo, rm);

    let rm = cpu.regs.r[rm];
    let rs = cpu.regs.r[rs];

    let res = if !U {
        let res = rm as u64 * rs as u64;
        if !A {
            res
        } else {
            let acc = cpu.regs.r[rdlo] as u64 | (cpu.regs.r[rdhi] as u64) << 32;
            res.wrapping_add(acc)
        }
    } else {
        let res = rm as i32 as i64 * rs as i32 as i64;
        let res = if !A {
            res
        } else {
            let acc = cpu.regs.r[rdlo] as i64 | (cpu.regs.r[rdhi] as i64) << 32;
            res.wrapping_add(acc)
        };
        res as u64
    };

    cpu.regs.r[rdhi] = (res >> 32) as u32;
    cpu.regs.r[rdlo] = res as u32;

    if S {
        cpu.regs.n_flag = (res >> 63) != 0;
        cpu.regs.z_flag = res == 0;
    }
}

fn arm_disasm_mull(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let rdhi = (instr >> 16) & 0xF;
    let rdlo = (instr >> 12) & 0xF;
    let rs = (instr >> 8) & 0xF;
    let rm = instr & 0xF;
    let u = (instr >> 26) & 1;
    let u = if u == 0 { "s" } else { "u" };
    let a = (instr >> 25) & 1;
    let s = (instr >> 24) & 1;
    let s = if s == 0 { "" } else { "s" };

    assert_ne!(rdhi, rdlo);
    assert_ne!(rdhi, rm);
    assert_ne!(rdlo, rm);
    assert_ne!(rdhi, 15);
    assert_ne!(rdlo, 15);

    let mne = if a == 0 { "mul" } else { "mla" };
    format!("{u}{mne}{cond}l{s} r{rdlo}, r{rdhi}, r{rm}, r{rs}")
}

trait Data {
    fn load(ctx: &mut impl Context, addr: u32, first: bool) -> u32;
    fn store(_ctx: &mut impl Context, _addr: u32, _data: u32, _first: bool) {
        panic!()
    }
}

impl Data for u32 {
    fn load(ctx: &mut impl Context, addr: u32, first: bool) -> u32 {
        let ofs = addr & 3;
        let data = ctx.read32(addr & !3, first);
        data.rotate_right(ofs * 8)
    }

    fn store(ctx: &mut impl Context, addr: u32, data: u32, first: bool) {
        ctx.write32(addr & !3, data, first)
    }
}

impl Data for u16 {
    fn load(ctx: &mut impl Context, addr: u32, first: bool) -> u32 {
        let ofs = addr & 1;
        let data = ctx.read16(addr & !1, first) as u32;
        data.rotate_right(ofs * 8)
    }

    fn store(ctx: &mut impl Context, addr: u32, data: u32, first: bool) {
        ctx.write16(addr & !1, data as u16, first)
    }
}

impl Data for i16 {
    fn load(ctx: &mut impl Context, addr: u32, first: bool) -> u32 {
        let ofs = addr & 1;
        let data = ctx.read16(addr & !1, first) as i16;
        (data >> (ofs * 8)) as u32
    }
}

impl Data for u8 {
    fn load(ctx: &mut impl Context, addr: u32, first: bool) -> u32 {
        ctx.read8(addr, first) as u32
    }

    fn store(ctx: &mut impl Context, addr: u32, data: u32, first: bool) {
        ctx.write8(addr, data as u8, first)
    }
}

impl Data for i8 {
    fn load(ctx: &mut impl Context, addr: u32, first: bool) -> u32 {
        ctx.read8(addr, first) as i8 as u32
    }
}

fn arm_op_ldst<
    C: Context,
    const I: bool,
    const P: bool,
    const U: bool,
    const B: bool,
    const W: bool,
    const L: bool,
>(
    cpu: &mut Cpu<C>,
    ctx: &mut C,
    instr: u32,
) {
    // TODO: non-previllege mode access

    let rn = ((instr >> 16) & 0xF) as usize;
    let rd = ((instr >> 12) & 0xF) as usize;

    // TODO: check
    // * R15 must not be specified as the register offset (Rm).

    let offset = if !I {
        instr & 0xFFF
    } else {
        decode_reg_sft_imm(cpu, instr).0
    };

    let base = cpu.regs.r[rn];
    let ea = if U {
        base.wrapping_add(offset)
    } else {
        base.wrapping_sub(offset)
    };

    let addr = if P { ea } else { base };

    if L {
        if B {
            cpu.regs.r[rd] = u8::load(ctx, addr, true);
        } else {
            cpu.regs.r[rd] = u32::load(ctx, addr, true);
        };
    } else {
        // When R15 is the source register (Rd) of a register store (STR) instruction,
        // the stored value will be address of the instruction plus 12.
        let data = cpu.regs.r[rd].wrapping_add(if rd == 15 { 4 } else { 0 });
        if B {
            u8::store(ctx, addr, data, true);
        } else {
            u32::store(ctx, addr, data, true);
        }
    }

    cpu.fetch_first = true;

    if W || !P {
        // Write-back must not be specified if R15 is specified as the base register (Rn).
        assert_ne!(rn, 15);
        cpu.regs.r[rn] = ea;
    }
}

fn arm_disasm_ldst(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);

    let i = (instr >> 25) & 1;
    let pre = (instr >> 24) & 1;
    let up = (instr >> 23) & 1;
    let b = (instr >> 22) & 1;
    let wb = (instr >> 21) & 1;
    let ld = (instr >> 20) & 1;
    let rn = (instr >> 16) & 0xF;
    let rd = (instr >> 12) & 0xF;

    let b = if b == 0 { "" } else { "b" };
    let t = if pre == 0 && wb == 1 { "t" } else { "" };

    let mne = if ld == 0 { "str" } else { "ldr" };

    let sign = if up == 0 { "-" } else { "" };

    let addr = {
        let w = if wb == 0 { "" } else { "!" };
        if i == 0 {
            let ofs = instr & 0xFFF;
            if ofs == 0 {
                assert_eq!(w, "");
                assert_eq!(pre, 1);
                format!("[r{rn}]")
            } else {
                if pre == 1 {
                    format!("[r{rn}, #{sign}{ofs}]{w}")
                } else {
                    format!("[r{rn}], #{sign}{ofs}")
                }
            }
        } else {
            let rm = instr & 0xF;
            let shift_by_reg = (instr >> 4) & 1 != 0;

            assert!(!shift_by_reg);

            let ty = (instr >> 5) & 3;

            const SHIFT_TYPE: [&str; 4] = ["lsl", "lsr", "asr", "ror"];
            let ty = SHIFT_TYPE[ty as usize];

            let amo = (instr >> 7) & 0x1F;
            if amo == 0 {
                if pre == 1 {
                    format!("[r{rn}, #{sign}r{rm}]{w}")
                } else {
                    format!("[r{rn}], #{sign}r{rm}")
                }
            } else {
                if pre == 1 {
                    format!("[r{rn}, #{sign}r{rm}, {ty} #{amo}]{w}")
                } else {
                    format!("[r{rn}], #{sign}r{rm}, {ty} #{amo}")
                }
            }
        }
    };

    format!("{mne}{cond}{b}{t} r{rd}, {addr}")
}

fn arm_op_ldsth<
    C: Context,
    T: Data,
    const P: bool,
    const U: bool,
    const I: bool,
    const W: bool,
    const L: bool,
>(
    cpu: &mut Cpu<C>,
    ctx: &mut C,
    instr: u32,
) {
    // cycles:
    // * LDR: 1S + 1N + 1I (rd=PC, 2S + 2N + 1I)
    // * STR: 2N

    let rn = ((instr >> 16) & 0xF) as usize;
    let rd = ((instr >> 12) & 0xF) as usize;

    let base = cpu.regs.r[rn];

    let offset = if !I {
        // R15 must not be specified as the register offset (Rm).
        let rm = (instr & 0xF) as usize;
        assert_ne!(rm, 15);
        cpu.regs.r[rm]
    } else {
        (instr >> 4) & 0xF0 | instr & 0xF
    };

    let ea = if U {
        cpu.regs.r[rn].wrapping_add(offset)
    } else {
        cpu.regs.r[rn].wrapping_sub(offset)
    };

    let addr = if P { ea } else { base };

    if L {
        cpu.regs.r[rd] = T::load(ctx, addr, true);
    } else {
        // When R15 is the source register (Rd) of a register store (STR) instruction,
        // the stored value will be address of the instruction plus 12.
        let data = cpu.regs.r[rd].wrapping_add(if rd == 15 { 4 } else { 0 });
        T::store(ctx, addr, data, true);
    }

    cpu.fetch_first = true;

    if W || !P {
        // Write-back should not be specified if R15 is specified as the base register (Rn).
        assert_ne!(rn, 15);
        cpu.regs.r[rn] = ea;
    }
}

fn arm_disasm_ldsth(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);

    let pre = (instr >> 24) & 1;
    let up = (instr >> 23) & 1;
    let i = (instr >> 22) & 1;
    let wb = (instr >> 21) & 1;
    let ld = (instr >> 20) & 1;

    let rn = (instr >> 16) & 0xF;
    let rd = (instr >> 12) & 0xF;

    let sh = (instr >> 5) & 3;

    let ty = match sh {
        1 => "h",
        2 => "sb",
        3 => "sh",
        _ => unreachable!(),
    };

    let mne = if ld == 0 { "str" } else { "ldr" };
    let w = if wb == 0 { "" } else { "!" };
    let sign = if up == 0 { "-" } else { "+" };

    let addr = if i == 0 {
        let rm = instr & 0xF;
        if pre == 1 {
            format!("[r{rn}, {sign}r{rm}]{w}")
        } else {
            format!("[r{rn}], {sign}r{rm}")
        }
    } else {
        let ofs = (instr & 0xF) | (((instr >> 8) & 0xF) << 4);

        if pre == 1 {
            if ofs == 0 {
                assert!(w == "");
                format!("[r{rn}]")
            } else {
                format!("[r{rn}, {sign}#0x{ofs}]{w}")
            }
        } else {
            format!("[r{rn}], {sign}#0x{ofs}")
        }
    };

    format!("{mne}{cond}{ty} r{rd}, {addr}")
}

fn arm_op_ldstm<
    C: Context,
    const PRE: bool,
    const UP: bool,
    const S: bool,
    const W: bool,
    const L: bool,
>(
    cpu: &mut Cpu<C>,
    ctx: &mut C,
    instr: u32,
) {
    // FIXME: implement correct cycle
    // LDM: nS + 1N + 1I cycles (LDM PC: (n+1)S + 2N + 1I)
    // STM: (n-1)S + 2N cycles

    let rn = ((instr >> 16) & 0xF) as usize;

    // R15 should not be used as the base register in any LDM or STM instruction.
    assert_ne!(rn, 15);

    let base = cpu.regs.r[rn];
    let cnt = (instr & 0xFFFF).count_ones();
    let end = if UP {
        base.wrapping_add(cnt * 4)
    } else {
        base.wrapping_sub(cnt * 4)
    };
    let start = if UP {
        if PRE {
            base.wrapping_add(4)
        } else {
            base
        }
    } else {
        if PRE {
            end
        } else {
            end.wrapping_add(4)
        }
    };

    // trace!("arm_op_ldstm: base:{base:08x}, pre:{PRE}, up:{UP}, wb:{W}, s:{S}");

    let cur_mode = cpu.regs.mode;

    let user_bank = if S {
        assert!(cur_mode != MODE_USER);

        if instr & (1 << 15) != 0 {
            if L {
                // LDM with R15
                cpu.regs.set_cpsr(cpu.regs.spsr);
                false
            } else {
                // STM with R15
                true
            }
        } else {
            // R15 not in list
            true
        }
    } else {
        false
    };

    if user_bank {
        cpu.regs.change_mode(MODE_USER);
    }

    // FIXME:
    // When write-back is specified, the base is written back at the end of the second cycle
    // of the instruction. During a STM, the first register is written out at the start of the
    // second cycle. A STM which includes storing the base, with the base as the first register
    // to be stored, will therefore store the unchanged value, whereas with the base second
    // or later in the transfer order, will store the modified value. A LDM will always overwrite
    // the updated base if the base is in the list.

    let mut addr = start;
    let mut first = true;

    for i in 0..16 {
        if instr & (1 << i) == 0 {
            continue;
        }

        if L {
            let data = u32::load(ctx, addr, first);
            // trace!("LDM[{i:02}]: 0x{addr:08X} = 0x{data:08X}");
            if i != 15 {
                cpu.regs.r[i] = data;
            } else {
                cpu.set_pc(data);
            }
        } else {
            // Whenever R15 is stored to memory the stored value is the address of the STM
            // instruction plus 12.
            let data = cpu.regs.r[i].wrapping_add(if i == 15 { 4 } else { 0 });
            // trace!("STM[{i:02}]: 0x{addr:08X} = 0x{data:08X}");
            u32::store(ctx, addr, data, first);
        }
        first = false;

        addr = addr.wrapping_add(4)
    }

    if user_bank {
        cpu.regs.change_mode(cur_mode);
    }

    if !first {
        cpu.fetch_first = true;
    }

    if W {
        assert!(!user_bank);
        cpu.regs.r[rn] = end;
    }
}

fn arm_disasm_ldstm(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);

    let pre = (instr >> 24) & 1;
    let up = (instr >> 23) & 1;
    let s = (instr >> 22) & 1;
    let wb = (instr >> 21) & 1;
    let ld = (instr >> 20) & 1;
    let rn = (instr >> 16) & 0xF;

    let mne = if ld == 0 { "stm" } else { "ldm" };

    let suf = match (ld, pre, up) {
        (1, 1, 1) => "ed",
        (1, 0, 1) => "fd",
        (1, 1, 0) => "ea",
        (1, 0, 0) => "fa",
        (0, 1, 1) => "fa",
        (0, 0, 1) => "ea",
        (0, 1, 0) => "fd",
        (0, 0, 0) => "ed",
        _ => unreachable!(),
    };

    let wb = if wb == 0 { "" } else { "!" };

    let rlist = fmt_reglist(instr as u16);
    let cpsr = if s == 0 { "" } else { "^" };

    format!("{mne}{cond}{suf} r{rn}{wb}, {rlist}{cpsr}")
}

fn fmt_reglist(reglist: u16) -> String {
    use std::fmt::Write;

    let mut ret = String::new();
    let reglist = reglist.view_bits::<Lsb0>();

    write!(&mut ret, "{{").unwrap();

    let mut first = true;

    for i in 0..16 {
        if !(reglist[i] && (i == 0 || !reglist[i - 1])) {
            continue;
        }

        let mut j = i;
        while j + 1 < 16 && reglist[j + 1] {
            j += 1;
        }

        if first {
            first = false;
        } else {
            write!(&mut ret, ", ").unwrap();
        }

        if i == j {
            write!(&mut ret, "r{i}").unwrap();
        } else if j - i == 1 {
            write!(&mut ret, "r{i}, r{j}").unwrap();
        } else {
            write!(&mut ret, "r{i}-r{j}").unwrap();
        }
    }

    write!(&mut ret, "}}").unwrap();

    ret
}

fn arm_op_swp<C: Context, const B: bool>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u32) {
    let rn = ((instr >> 16) & 0xF) as usize;
    let rd = ((instr >> 12) & 0xF) as usize;
    let rm = (instr & 0xF) as usize;

    // Do not use R15 as an operand (Rd, Rn or Rs) in a SWP instruction.
    assert!(rd != 15);
    assert!(rn != 15);
    assert!(rm != 15);

    let addr = cpu.regs.r[rn];
    let src = cpu.regs.r[rm];
    let data = if !B {
        u32::load(ctx, addr, true)
    } else {
        u8::load(ctx, addr, true)
    };
    if !B {
        u32::store(ctx, addr, src, true);
    } else {
        u8::store(ctx, addr, src, true);
    }
    cpu.regs.r[rd] = data;
    cpu.fetch_first = true;
}

fn arm_disasm_swp(instr: u32, _pc: u32) -> String {
    // cccc 0001 0B00 nnnn dddd 0000 1001 mmmm
    assert_eq!(instr & 0x0FB00FF0, 0x01000090);
    let cond = disasm_cond(instr);
    let b = (instr >> 22) & 1;
    let rn = (instr >> 16) & 0xF;
    let rd = (instr >> 12) & 0xF;
    let rm = instr & 0xF;

    let b = if b == 0 { "" } else { "b" };
    format!("swp{cond}{b} r{rd}, r{rm}, [r{rn}]")
}

fn arm_op_swi<C: Context>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u32) {
    info!("SWI: {instr:08X}, cycle: {}", ctx.now());
    cpu.exception(Exception::SoftwareInterrupt)
}

fn arm_disasm_swi(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let comment = instr & 0xFFFFFF;
    format!("swi{cond} #0x{comment:06X}")
}

fn arm_op_ldstc<
    C: Context,
    const P: bool,
    const U: bool,
    const N: bool,
    const W: bool,
    const L: bool,
>(
    _cpu: &mut Cpu<C>,
    _ctx: &mut C,
    _instr: u32,
) {
    todo!()
}

fn arm_disasm_ldstc(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let pre = (instr >> 24) & 1;
    let up = (instr >> 23) & 1;
    let len = (instr >> 22) & 1;
    let wb = (instr >> 21) & 1;
    let ld = (instr >> 20) & 1;
    let rn = (instr >> 16) & 0xF;
    let crd = (instr >> 12) & 0xF;
    let cp_num = (instr >> 8) & 0xF;
    let offset = instr & 0xFF;

    let mne = if ld == 0 { "stc" } else { "ldc" };
    let l = if len == 0 { "" } else { "l" };
    let sign = if up == 0 { "-" } else { "+" };
    let wb = if wb == 0 { "" } else { "!" };

    let addr = if pre == 1 {
        if offset == 0 {
            format!("[r{rn}]")
        } else {
            format!("[r{rn}, {sign}#0x{offset:02X}]{wb}")
        }
    } else {
        format!("[r{rn}], {sign}#0x{offset:02X}")
    };

    return format!("{mne}{cond}{l} p{cp_num}, c{crd}, {addr}");
}

fn arm_op_cdp<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, _instr: u32) {
    todo!()
}

fn arm_disasm_cdp(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let cp_opc = (instr >> 20) & 0xF;
    let crn = (instr >> 16) & 0xF;
    let crd = (instr >> 12) & 0xF;
    let cp_num = (instr >> 8) & 0xF;
    let cp = (instr >> 5) & 7;
    let crm = instr & 0xF;

    let expr2 = if cp == 0 {
        "".to_string()
    } else {
        format!(", {cp}")
    };
    format!("cdp{cond} p{cp_num}, {cp_opc}, c{crd}, c{crn}, c{crm}{expr2}")
}

fn arm_op_mrc<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, _instr: u32) {
    todo!()
}

fn arm_op_mcr<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, _instr: u32) {
    todo!()
}

fn arm_disasm_mrc_mcr(instr: u32, _pc: u32) -> String {
    let cond = disasm_cond(instr);
    let cp_opc = (instr >> 21) & 7;
    let ld = (instr >> 20) & 1;
    let crn = (instr >> 16) & 0xF;
    let rd = (instr >> 12) & 0xF;
    let cp_num = (instr >> 8) & 0xF;
    let cp = (instr >> 5) & 7;
    let crm = instr & 0xF;

    let mne = if ld == 0 { "mcr" } else { "mrc" };

    let expr2 = if cp == 0 {
        "".to_string()
    } else {
        format!(", {cp}")
    };
    return format!("{mne}{cond} p{cp_num}, {cp_opc}, r{rd}, c{crn}, c{crm}{expr2}");
}

fn arm_op_undef<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    trace!("Undefined instruction: {:08X}", instr);
    panic!()
}

fn arm_disasm_undef(_instr: u32, _pc: u32) -> String {
    "undef".to_string()
}

fn arm_op_invalid<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    trace!("Invalid instruction: {:08X}", instr);
    panic!()
}

fn arm_disasm_invalid(_instr: u32, _pc: u32) -> String {
    "invalid".to_string()
}

fn thumb_op_invalid<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    trace!("Invalid instruction: {:04X}", instr);
    panic!()
}

impl<C: Context> Cpu<C> {
    fn dump_regs(&mut self) -> String {
        use std::fmt::Write;

        let prev_regs = self.prev_regs;

        let mode = mode_name(self.regs.mode);

        let mut ret = String::new();
        write!(
            &mut ret,
            "M:{mode} {n}{z}{c}{v}{i}{f}{t}",
            n = if self.regs.n_flag { 'N' } else { 'n' },
            z = if self.regs.z_flag { 'Z' } else { 'z' },
            c = if self.regs.c_flag { 'C' } else { 'c' },
            v = if self.regs.v_flag { 'V' } else { 'v' },
            i = if self.regs.irq_disable { 'I' } else { 'i' },
            f = if self.regs.fiq_disable { 'F' } else { 'f' },
            t = if self.regs.state { 'T' } else { 't' },
        )
        .unwrap();

        for i in 0..=14 {
            if prev_regs[i] != self.regs.r[i] {
                write!(&mut ret, " r{i}:{:08X}", self.regs.r[i]).unwrap();
            }
        }

        self.prev_regs = self.regs.r;
        ret
    }

    fn disasm_arm(&self, instr: u32, pc: u32) -> String {
        let ix = (instr >> 16) & 0xFF0 | (instr >> 4) & 0xF;
        self.arm_disasm_table[ix as usize](instr, pc)
    }

    fn disasm_thumb(&self, instr: u16, pc: u32) -> String {
        let ix = instr >> 6;
        self.thumb_disasm_table[ix as usize](instr, pc)
    }
}

fn thumb_op_shift<C: Context, const OP: u32>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    let offset = (instr >> 6) & 0x1F;
    let rs = ((instr >> 3) & 7) as usize;
    let rd = (instr & 7) as usize;

    // FIXME: is this correct?
    let (res, carry) = if OP == 0 && offset == 0 {
        (cpu.regs.r[rs], cpu.regs.c_flag)
    } else {
        let offset = if offset == 0 { 32 } else { offset };
        calc_sft(OP, cpu.regs.r[rs], offset as u8)
    };

    cpu.regs.c_flag = carry;
    cpu.regs.set_nz(res);
    cpu.regs.r[rd] = res;
}

fn thumb_disasm_shift(instr: u16, _pc: u32) -> String {
    let op = (instr >> 11) & 3;
    let offset = (instr >> 6) & 0x1F;
    let rs = (instr >> 3) & 7;
    let rd = instr & 7;

    let mne = match op {
        0 => "lsl",
        1 => "lsr",
        2 => "asr",
        _ => unreachable!(),
    };
    format!("{mne} r{rd}, r{rs}, #{offset}")
}

fn thumb_op_add_sub<C: Context, const I: bool, const OP: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u16,
) {
    let rs = ((instr >> 3) & 7) as usize;
    let rd = (instr & 7) as usize;

    let op1 = cpu.regs.r[rs];
    let op2 = if !I {
        let rn = ((instr >> 6) & 7) as usize;
        cpu.regs.r[rn]
    } else {
        ((instr >> 6) & 7) as u32
    };

    let res = if !OP {
        alu::<C, 0b0100>(cpu, op1, op2, cpu.regs.c_flag, true)
    } else {
        alu::<C, 0b0010>(cpu, op1, op2, cpu.regs.c_flag, true)
    };

    cpu.regs.r[rd] = res.unwrap();
}

fn thumb_disasm_add_sub(instr: u16, _pc: u32) -> String {
    let imm = (instr >> 10) & 1 != 0;
    let op = (instr >> 9) & 1;
    let rs = (instr >> 3) & 7;
    let rd = instr & 7;
    let mne = if op == 0 { "add" } else { "sub" };

    if imm {
        let ofs = (instr >> 6) & 7;
        format!("{mne} r{rd}, r{rs}, #{ofs}")
    } else {
        let rn = (instr >> 6) & 7;
        format!("{mne} r{rd}, r{rs}, r{rn}")
    }
}

fn thumb_op_alu_imm8<C: Context, const OP: usize>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    let rd = ((instr >> 8) & 7) as usize;
    let op1 = cpu.regs.r[rd];
    let op2 = (instr & 0xFF) as u32;
    let res = match OP {
        0 => alu::<C, 0b1101>(cpu, op1, op2, cpu.regs.c_flag, true),
        1 => alu::<C, 0b1010>(cpu, op1, op2, cpu.regs.c_flag, true),
        2 => alu::<C, 0b0100>(cpu, op1, op2, cpu.regs.c_flag, true),
        3 => alu::<C, 0b0010>(cpu, op1, op2, cpu.regs.c_flag, true),
        _ => unreachable!(),
    };
    if let Some(res) = res {
        cpu.regs.r[rd] = res;
    }
}

fn thumb_disasm_alu_imm8(instr: u16, _pc: u32) -> String {
    let op = (instr >> 11) & 3;
    let rd = (instr >> 8) & 7;
    let ofs = instr & 0xFF;
    let mne = match op {
        0 => "mov",
        1 => "cmp",
        2 => "add",
        3 => "sub",
        _ => unreachable!(),
    };
    format!("{mne} r{rd}, #{ofs}")
}

fn thumb_op_alu<C: Context, const OP: usize>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    let rs = ((instr >> 3) & 7) as usize;
    let rd = (instr & 7) as usize;

    let op1 = cpu.regs.r[rd];
    let op2 = cpu.regs.r[rs];

    let sft = |ty: u32| {
        let amount = op2 as u8;
        if amount == 0 {
            (op1, cpu.regs.c_flag)
        } else {
            calc_sft(ty, op1, amount)
        }
    };

    let res = match OP {
        0b0000 => alu::<C, 0b0000>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b0001 => alu::<C, 0b0001>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b0010 => {
            let (op2, carry) = sft(0);
            cpu.regs.c_flag = carry;
            alu::<C, 0b1101>(cpu, op1, op2, cpu.regs.c_flag, true)
        }
        0b0011 => {
            let (op2, carry) = sft(1);
            cpu.regs.c_flag = carry;
            alu::<C, 0b1101>(cpu, op1, op2, cpu.regs.c_flag, true)
        }
        0b0100 => {
            let (op2, carry) = sft(2);
            cpu.regs.c_flag = carry;
            alu::<C, 0b1101>(cpu, op1, op2, cpu.regs.c_flag, true)
        }
        0b0101 => alu::<C, 0b0101>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b0110 => alu::<C, 0b0110>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b0111 => {
            let (op2, carry) = sft(3);
            cpu.regs.c_flag = carry;
            alu::<C, 0b1101>(cpu, op1, op2, cpu.regs.c_flag, true)
        }
        0b1000 => alu::<C, 0b1000>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b1001 => alu::<C, 0b0011>(cpu, op2, 0, cpu.regs.c_flag, true),
        0b1010 => alu::<C, 0b1010>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b1011 => alu::<C, 0b1011>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b1100 => alu::<C, 0b1100>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b1101 => {
            let res = op1.wrapping_mul(op2);
            cpu.regs.set_nz(res);
            Some(res)
        }
        0b1110 => alu::<C, 0b1110>(cpu, op1, op2, cpu.regs.c_flag, true),
        0b1111 => alu::<C, 0b1111>(cpu, op1, op2, cpu.regs.c_flag, true),
        _ => unreachable!(),
    };

    if let Some(res) = res {
        cpu.regs.r[rd] = res;
    }
}

fn thumb_disasm_alu(instr: u16, _pc: u32) -> String {
    let op = (instr >> 6) & 0x1F;
    let rs = (instr >> 3) & 7;
    let rd = instr & 7;
    let mne = match op {
        0 => "and",
        1 => "eor",
        2 => "lsl",
        3 => "lsr",
        4 => "asr",
        5 => "adc",
        6 => "sbc",
        7 => "ror",
        8 => "tst",
        9 => "neg",
        10 => "cmp",
        11 => "cmn",
        12 => "orr",
        13 => "mul",
        14 => "bic",
        15 => "mvn",
        _ => unreachable!(),
    };
    format!("{mne} r{rd}, r{rs}")
}

fn thumb_op_hireg<C: Context, const OP: usize, const H1: bool, const H2: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u16,
) {
    // If R15 is used as an operand, the value will be the address of the instruction + 4 with
    // bit 0 cleared.

    let rs = (H2 as usize * 8) + ((instr >> 3) & 7) as usize;
    let rd = (H1 as usize * 8) + (instr & 7) as usize;

    let op1 = cpu.regs.r[rd] + if rd == 15 { 2 } else { 0 };
    let op2 = cpu.regs.r[rs] + if rs == 15 { 2 } else { 0 };

    let res = match OP {
        0 => alu::<C, 0b0100>(cpu, op1, op2, cpu.regs.c_flag, false),
        1 => alu::<C, 0b1010>(cpu, op1, op2, cpu.regs.c_flag, true),
        2 => alu::<C, 0b1101>(cpu, op1, op2, cpu.regs.c_flag, false),
        _ => unreachable!(),
    };

    if let Some(res) = res {
        if rd != 15 {
            cpu.regs.r[rd] = res;
        } else {
            cpu.set_pc(res & !1);
        }
    }
}

fn thumb_op_bx<C: Context, const H: bool>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    let rs = (H as usize * 8) + ((instr >> 3) & 7) as usize;
    let new_pc = cpu.regs.r[rs] + if rs == 15 { 2 } else { 0 };

    cpu.regs.state = new_pc & 1 != 0;

    // Executing a BX PC in THUMB state from a non-word aligned address
    // will result in unpredictable execution.
    cpu.set_pc(new_pc & !1);
}

fn thumb_disasm_hireg(instr: u16, _pc: u32) -> String {
    let op = (instr >> 8) & 3;
    let h1 = (instr >> 7) & 1;
    let h2 = (instr >> 6) & 1;
    let rs = h2 * 8 + ((instr >> 3) & 7);
    let rd = h1 * 8 + (instr & 7);

    let mne = match op {
        0 => "add",
        1 => "cmp",
        2 => "mov",
        3 => "bx",
        _ => unreachable!(),
    };

    if mne != "bx" {
        format!("{mne} r{rd}, r{rs}")
    } else {
        format!("{mne} r{rs}")
    }
}

fn thumb_op_ld_pcrel<C: Context>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u16) {
    let rd = ((instr >> 8) & 7) as usize;
    let imm = (instr & 0xFF) * 4;

    // The value of the PC will be 4 bytes greater than the address of this instruction,
    // but bit 1 of the PC is forced to 0 to ensure it is word aligned.
    let addr = cpu.regs.r[15].wrapping_add(imm as u32 + 2) & !3;
    cpu.regs.r[rd] = u32::load(ctx, addr, true);
    cpu.fetch_first = true;
}

fn thumb_disasm_ld_pcrel(instr: u16, _pc: u32) -> String {
    let rd = (instr >> 8) & 7;
    let imm = (instr & 0xFF) * 4;
    format!("ldr r{rd}, [pc, #{imm}]")
}

fn thumb_op_ldst<C: Context, const L: bool, T: Data>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u16) {
    let ro = ((instr >> 6) & 7) as usize;
    let rb = ((instr >> 3) & 7) as usize;
    let rd = (instr & 7) as usize;

    let addr = cpu.regs.r[rb].wrapping_add(cpu.regs.r[ro]);

    if L {
        cpu.regs.r[rd] = T::load(ctx, addr, true);
    } else {
        T::store(ctx, addr, cpu.regs.r[rd], true);
    }
    cpu.fetch_first = true;
}

fn thumb_disasm_ldst(instr: u16, _pc: u32) -> String {
    let ro = ((instr >> 6) & 7) as usize;
    let rb = ((instr >> 3) & 7) as usize;
    let rd = (instr & 7) as usize;

    let mne = if (instr >> 9) & 1 == 0 {
        let l = (instr >> 11) & 1 != 0;
        let b = (instr >> 10) & 1 != 0;
        match (l, b) {
            (false, false) => "str",
            (false, true) => "strb",
            (true, false) => "ldr",
            (true, true) => "ldrb",
        }
    } else {
        let h = (instr >> 11) & 1 != 0;
        let s = (instr >> 10) & 1 != 0;
        match (s, h) {
            (false, false) => "strh",
            (false, true) => "ldrh",
            (true, false) => "ldsb",
            (true, true) => "ldsh",
        }
    };

    format!("{mne} r{rd}, [r{rb}, r{ro}]")
}

fn thumb_op_ldst_ofs<C: Context, const L: bool, T: Data>(
    cpu: &mut Cpu<C>,
    ctx: &mut C,
    instr: u16,
) {
    let ofs = ((instr >> 6) & 0x1F) as u32;
    let rb = ((instr >> 3) & 7) as usize;
    let rd = (instr & 7) as usize;

    let addr = cpu.regs.r[rb].wrapping_add(ofs as u32 * size_of::<T>() as u32);
    if L {
        cpu.regs.r[rd] = T::load(ctx, addr, true);
    } else {
        T::store(ctx, addr, cpu.regs.r[rd], true);
    }
    cpu.fetch_first = true;
}

fn thumb_disasm_ldst_ofs(instr: u16, _pc: u32) -> String {
    let ofs = ((instr >> 6) & 0x1F) as u32;
    let rb = ((instr >> 3) & 7) as usize;
    let rd = (instr & 7) as usize;

    let (mne, suf, size) = if (instr >> 13) == 0b011 {
        let b = (instr >> 12) & 1 != 0;
        let l = (instr >> 11) & 1 != 0;
        let mne = if l { "ldr" } else { "str" };
        let suf = if b { "b" } else { "" };
        let size = if b { 1 } else { 4 };
        (mne, suf, size)
    } else {
        let l = (instr >> 11) & 1 != 0;
        (if l { "ldr" } else { "str" }, "h", 2)
    };

    let ofs = ofs * size;
    format!("{mne}{suf} r{rd}, [r{rb}, #{ofs}]")
}

fn thumb_op_ldst_sprel<C: Context, const L: bool>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u16) {
    let rd = ((instr >> 8) & 7) as usize;
    let ofs = (instr & 0xFF) as u32 * 4;
    let addr = cpu.regs.r[13].wrapping_add(ofs);
    if L {
        cpu.regs.r[rd] = u32::load(ctx, addr, true);
    } else {
        u32::store(ctx, addr, cpu.regs.r[rd], true);
    }
    cpu.fetch_first = true;
}

fn thumb_disasm_ldst_sprel(instr: u16, _pc: u32) -> String {
    let l = (instr >> 11) & 1 != 0;
    let rd = ((instr >> 8) & 7) as usize;
    let ofs = (instr & 0xFF) as u32 * 4;
    let mne = if l { "ldr" } else { "str" };
    format!("{mne} r{rd}, [sp, #{ofs}]")
}

fn thumb_op_load_addr<C: Context, const SP: bool>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    let rd = ((instr >> 8) & 7) as usize;
    let ofs = (instr & 0xFF) as u32 * 4;
    let addr = if SP {
        cpu.regs.r[13].wrapping_add(ofs)
    } else {
        // Where the PC is used as the source register (SP = 0), bit 1 of the PC is always read
        // as 0. The value of the PC will be 4 bytes greater than the address of the instruction
        // before bit 1 is forced to 0.
        cpu.regs.r[15].wrapping_add(ofs + 2) & !3
    };
    cpu.regs.r[rd] = addr;
}

fn thumb_disasm_load_addr(instr: u16, _pc: u32) -> String {
    let sp = (instr >> 11) & 1 != 0;
    let rd = ((instr >> 8) & 7) as usize;
    let ofs = (instr & 0xFF) as u32 * 4;
    format!("add r{rd}, r{}, #{ofs}", if sp { 13 } else { 15 })
}

fn thumb_op_add_sp<C: Context, const SIGN: bool>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    let ofs = (instr & 0x7F) as u32 * 4;
    if !SIGN {
        cpu.regs.r[13] = cpu.regs.r[13].wrapping_add(ofs);
    } else {
        cpu.regs.r[13] = cpu.regs.r[13].wrapping_sub(ofs);
    }
}

fn thumb_disasm_add_sp(instr: u16, _pc: u32) -> String {
    let sign = (instr >> 7) & 1 != 0;
    let ofs = (instr & 0x7F) as u32 * 4;
    let mne = if sign { "sub" } else { "add" };
    let sign = if sign { "-" } else { "" };
    format!("{mne} sp, #{sign}{ofs}")
}

fn block_trans<C: Context, const L: bool, const IA: bool, const R: bool>(
    cpu: &mut Cpu<C>,
    ctx: &mut C,
    rb: usize,
    rlist: u16,
) {
    let cnt = rlist.count_ones() + R as u32;

    let base = cpu.regs.r[rb];
    let end = if IA {
        base.wrapping_add(cnt * 4)
    } else {
        base.wrapping_sub(cnt * 4)
    };
    let start = if IA { base } else { end };

    // Writeback with Rb included in Rlist:
    // Store OLD base if Rb is FIRST entry in Rlist, otherwise store NEW base (STM/ARMv4), always store OLD base (STM/ARMv5), no writeback (LDM/ARMv4/ARMv5; at this point, THUMB opcodes work different than ARM opcodes).

    let mut addr = start;
    let mut first = true;
    for i in 0..8 {
        if rlist & (1 << i) == 0 {
            continue;
        }
        if !L {
            let data = if i == rb && !first {
                end
            } else {
                cpu.regs.r[i]
            };
            u32::store(ctx, addr, data, first);
        } else {
            cpu.regs.r[i] = u32::load(ctx, addr, first);
        }
        first = false;
        addr = addr.wrapping_add(4);
    }
    if R {
        if !L {
            u32::store(ctx, addr, cpu.regs.r[14], first);
        } else {
            cpu.set_pc(u32::load(ctx, addr, first) & !1);
        }
        first = false;
    }
    if !first {
        cpu.fetch_first = true;
    }

    if !(rlist & (1 << rb) != 0 && L) {
        cpu.regs.r[rb] = end;
    }
}

fn thumb_op_push_pop<C: Context, const L: bool, const R: bool>(
    cpu: &mut Cpu<C>,
    ctx: &mut C,
    instr: u16,
) {
    block_trans::<C, L, L, R>(cpu, ctx, 13, instr & 0xFF)
}

fn thumb_disasm_push_pop(instr: u16, _pc: u32) -> String {
    let l = (instr >> 11) & 1 != 0;
    let r = (instr >> 8) & 1 != 0;
    let mne = if l { "pop" } else { "push" };
    let rlist = instr & 0xFF;
    let pclr = if r { 1 << if l { 15 } else { 14 } } else { 0 };
    format!("{mne} {}", fmt_reglist(rlist | pclr))
}

fn thumb_op_ldstm<C: Context, const L: bool>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u16) {
    let rb = ((instr >> 8) & 7) as usize;
    let rlist = instr & 0xFF;

    // Empty Rlist: R15 loaded/stored (ARMv4 only), and Rb=Rb+40h (ARMv4-v5).
    if rlist == 0 {
        if L {
            cpu.set_pc(u32::load(ctx, cpu.regs.r[rb], true));
        } else {
            u32::store(ctx, cpu.regs.r[rb], cpu.regs.r[15].wrapping_add(4), true);
        }
        cpu.regs.r[rb] = cpu.regs.r[rb].wrapping_add(0x40);
    } else {
        block_trans::<C, L, true, false>(cpu, ctx, rb, rlist);
    }
}

fn thumb_disasm_ldstm(instr: u16, _pc: u32) -> String {
    let l = (instr >> 11) & 1 != 0;
    let rb = ((instr >> 8) & 7) as usize;
    let mne = if l { "ldm" } else { "stm" };
    let rlist = instr & 0xFF;
    format!("{mne}ia r{rb}!, {}", fmt_reglist(rlist))
}

fn thumb_op_cond_branch<C: Context, const COND: usize>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    if cpu.regs.check_cond(COND as u8) {
        let offset = (instr as i8) as i32 * 2 + 2;
        cpu.set_pc(cpu.regs.r[15].wrapping_add(offset as u32));
    }
}

fn thumb_disasm_cond_branch(instr: u16, pc: u32) -> String {
    trace!("thumb_disasm_cond_branch: PC:{pc:08X}, instr:{instr:04X}");

    let cond = (instr >> 8) & 0xF;
    let offset = ((instr as i8) as i32 * 2 + 4) as u32;
    let dest = pc.wrapping_add(offset);
    format!("b{} #0x{dest:08X}", COND[cond as usize])
}

fn thumb_op_swi<C: Context>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u16) {
    info!("SWI: {:04X}, cycle: {}", instr, ctx.now());
    cpu.exception(Exception::SoftwareInterrupt);
}

fn thumb_disasm_swi(instr: u16, _pc: u32) -> String {
    let value = instr & 0xFF;
    format!("swi {value}")
}

fn thumb_op_b<C: Context>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    let offset = ((((instr as u32) << 21) as i32) >> 21) * 2 + 2;
    let dest = cpu.regs.r[15].wrapping_add(offset as u32);
    cpu.set_pc(dest);
}

fn thumb_disasm_b(instr: u16, pc: u32) -> String {
    let offset = ((((instr as u32) << 21) as i32) >> 21) * 2 + 4;
    let dest = pc.wrapping_add(offset as u32);
    format!("b 0x{dest:08X}")
}

fn thumb_op_bl<C: Context, const H: bool>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u16) {
    if !H {
        let offset = (((instr as u32) << 21) as i32) >> 21;
        let lr = cpu.regs.r[15]
            .wrapping_add(2)
            .wrapping_add((offset << 12) as u32);
        cpu.regs.r[14] = lr;
    } else {
        let offset = (instr & 0x7FF) as u32;
        let ret_addr = cpu.regs.r[15];
        let new_pc = cpu.regs.r[14].wrapping_add(offset * 2);
        trace!("BL 0x{:08X}", new_pc);
        cpu.set_pc(new_pc);
        cpu.regs.r[14] = ret_addr | 1;
    }
}

fn thumb_disasm_bl(instr: u16, _pc: u32) -> String {
    let h = (instr >> 11) & 1;
    let offset = instr & 0x7FF;
    if h == 0 {
        format!("bl1 {:03X}xxx", offset)
    } else {
        format!("bl2 xxx{:03X}", offset * 2)
    }
}

fn thumb_disasm_invalid(_instr: u16, _pc: u32) -> String {
    "invalid".to_string()
}
