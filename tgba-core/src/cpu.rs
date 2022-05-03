use bitvec::prelude::*;
use log::trace;

use crate::{
    context::{Bus, Interrupt},
    util::trait_alias,
};

trait_alias!(pub trait Context = Bus + Interrupt);

type ArmOp<C> = fn(&mut Cpu<C>, &mut C, u32);

pub struct Cpu<C: Context> {
    regs: Registers,
    pc_changed: bool,
    arm_op_table: [ArmOp<C>; 0x1000],
}

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
        Cpu {
            regs: Registers::default(),
            pc_changed: false,
            arm_op_table: build_arm_op_table(),
        }
    }

    pub fn set_pc(&mut self, pc: u32) {
        self.regs.r[15] = pc;
        self.pc_changed = true;
    }

    pub fn exec_one(&mut self, ctx: &mut C) {
        if !self.regs.fiq_disable && ctx.fiq() {
            self.exception(Exception::FIQ);
            return;
        }

        if !self.regs.irq_disable && ctx.irq() {
            self.exception(Exception::IRQ);
            return;
        }

        if !self.regs.state {
            self.exec_arm(ctx);
        } else {
            self.exec_thumb(ctx);
        }
    }

    fn exception(&mut self, e: Exception) {
        let old_cpsr = self.regs.cpsr();
        self.regs.change_mode(e.mode_on_entry());
        self.regs.spsr = old_cpsr;
        self.regs.r[14] = self.regs.r[15] - 4;
        self.regs.r[15] = e.vector_addr();
        self.regs.state = false;
    }

    fn exec_arm(&mut self, ctx: &mut C) {
        let instr = self.fetch32(ctx);

        if log::log_enabled!(log::Level::Trace) {
            let pc = self.regs.r[15].wrapping_sub(4);
            let s = disasm(instr, pc);
            trace!("{pc:08X}: {instr:08X}: {s}");
        }

        if self.regs.check_cond((instr >> 28) as u8) {
            self.regs.r[15] = self.regs.r[15].wrapping_add(4);
            self.pc_changed = false;

            let ix = (instr >> 16) & 0xFF0 | (instr >> 4) & 0xF;
            self.arm_op_table[ix as usize](self, ctx, instr);

            if !self.pc_changed {
                self.regs.r[15] = self.regs.r[15].wrapping_sub(4);
            }
        }
    }

    fn exec_thumb(&mut self, ctx: &mut impl Context) {
        todo!()
    }

    fn fetch32(&mut self, ctx: &mut impl Context) -> u32 {
        let pc = self.regs.r[15];
        assert_eq!(pc & 0x3, 0);

        self.regs.r[15] += 4;
        ctx.read32(pc)
    }
}

fn build_arm_op_table<C: Context>() -> [ArmOp<C>; 0x1000] {
    let mut tbl = [arm_op_invalid as ArmOp<C>; 0x1000];

    let invalid = |hi: usize, lo: usize| {
        trace!(
            "Invalid code: CCCC {:04b} {:04b} .... .... .... {:04b} ....",
            hi >> 4,
            hi & 0xF,
            lo
        );
        arm_op_invalid
    };

    for hi in 0..0x100 {
        macro_rules! ldsth {
            ($t:ty) => {
                match hi {
                    0b00000 => arm_op_ldsth::<C, u16, false, false, false, false, false>,
                    0b00001 => arm_op_ldsth::<C, u16, false, false, false, false, true>,
                    0b00010 => arm_op_ldsth::<C, u16, false, false, false, true, false>,
                    0b00011 => arm_op_ldsth::<C, u16, false, false, false, true, true>,
                    0b00100 => arm_op_ldsth::<C, u16, false, false, true, false, false>,
                    0b00101 => arm_op_ldsth::<C, u16, false, false, true, false, true>,
                    0b00110 => arm_op_ldsth::<C, u16, false, false, true, true, false>,
                    0b00111 => arm_op_ldsth::<C, u16, false, false, true, true, true>,
                    0b01000 => arm_op_ldsth::<C, u16, false, true, false, false, false>,
                    0b01001 => arm_op_ldsth::<C, u16, false, true, false, false, true>,
                    0b01010 => arm_op_ldsth::<C, u16, false, true, false, true, false>,
                    0b01011 => arm_op_ldsth::<C, u16, false, true, false, true, true>,
                    0b01100 => arm_op_ldsth::<C, u16, false, true, true, false, false>,
                    0b01101 => arm_op_ldsth::<C, u16, false, true, true, false, true>,
                    0b01110 => arm_op_ldsth::<C, u16, false, true, true, true, false>,
                    0b01111 => arm_op_ldsth::<C, u16, false, true, true, true, true>,
                    0b10000 => arm_op_ldsth::<C, u16, true, false, false, false, false>,
                    0b10001 => arm_op_ldsth::<C, u16, true, false, false, false, true>,
                    0b10010 => arm_op_ldsth::<C, u16, true, false, false, true, false>,
                    0b10011 => arm_op_ldsth::<C, u16, true, false, false, true, true>,
                    0b10100 => arm_op_ldsth::<C, u16, true, false, true, false, false>,
                    0b10101 => arm_op_ldsth::<C, u16, true, false, true, false, true>,
                    0b10110 => arm_op_ldsth::<C, u16, true, false, true, true, false>,
                    0b10111 => arm_op_ldsth::<C, u16, true, false, true, true, true>,
                    0b11000 => arm_op_ldsth::<C, u16, true, true, false, false, false>,
                    0b11001 => arm_op_ldsth::<C, u16, true, true, false, false, true>,
                    0b11010 => arm_op_ldsth::<C, u16, true, true, false, true, false>,
                    0b11011 => arm_op_ldsth::<C, u16, true, true, false, true, true>,
                    0b11100 => arm_op_ldsth::<C, u16, true, true, true, false, false>,
                    0b11101 => arm_op_ldsth::<C, u16, true, true, true, false, true>,
                    0b11110 => arm_op_ldsth::<C, u16, true, true, true, true, false>,
                    0b11111 => arm_op_ldsth::<C, u16, true, true, true, true, true>,
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
            tbl[(hi << 4) | lo] = match hi >> 5 {
                0b000 => {
                    if hi == 0b00010010 && lo == 0b0001 {
                        arm_op_bx
                    } else if lo == 0b1001 {
                        match hi {
                            0b000_00 => arm_op_mul::<C, false, false>,
                            0b000_01 => arm_op_mul::<C, false, true>,
                            0b000_10 => arm_op_mul::<C, true, false>,
                            0b000_11 => arm_op_mul::<C, true, true>,

                            0b01_000 => arm_op_mull::<C, false, false, false>,
                            0b01_001 => arm_op_mull::<C, false, false, true>,
                            0b01_010 => arm_op_mull::<C, false, true, false>,
                            0b01_011 => arm_op_mull::<C, false, true, true>,
                            0b01_100 => arm_op_mull::<C, true, false, false>,
                            0b01_101 => arm_op_mull::<C, true, false, true>,
                            0b01_110 => arm_op_mull::<C, true, true, false>,
                            0b01_111 => arm_op_mull::<C, true, true, true>,

                            0b10_0_00 => arm_op_swp::<C, false>,
                            0b10_1_00 => arm_op_swp::<C, true>,

                            _ => invalid(hi, lo),
                        }
                    } else if lo == 0b1011 {
                        ldsth!(u16)
                    } else if lo == 0b1101 {
                        ldsth!(i8)
                    } else if lo == 0b1111 {
                        ldsth!(i16)
                    } else if hi & 0b000_1100_1 == 0b000_1000_0 {
                        // compare with s = 0
                        if hi & 0b000_1101_1 == 0b000_1000_0 && lo == 0 {
                            if hi & 0b000_1111_0 == 0b000_1000_0 {
                                arm_op_mrs::<C, false>
                            } else {
                                arm_op_mrs::<C, true>
                            }
                        } else if hi & 0b000_1101_1 == 0b000_1001_0 {
                            if hi & 0b000_1111_1 == 0b000_1001_0 {
                                arm_op_msr::<C, false, false>
                            } else {
                                arm_op_msr::<C, false, true>
                            }
                        } else {
                            invalid(hi, lo)
                        }
                    } else {
                        alu!()
                    }
                }

                0b001 => {
                    if hi & 0b000_1100_1 == 0b000_1000_0 {
                        // compare with s = 0
                        if hi & 0b000_1101_1 == 0b000_1001_0 {
                            if hi & 0b000_1111_1 == 0b000_1001_0 {
                                arm_op_msr::<C, true, false>
                            } else {
                                arm_op_msr::<C, true, true>
                            }
                        } else {
                            invalid(hi, lo)
                        }
                    } else {
                        alu!()
                    }
                }

                0b011 if lo & 1 == 1 => arm_op_undef,

                0b010 | 0b011 => match hi {
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
                },

                0b100 => match hi {
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
                },

                0b101 => {
                    if (hi >> 4) & 1 == 0 {
                        arm_op_b::<C, false>
                    } else {
                        arm_op_b::<C, true>
                    }
                }

                0b110 => match hi {
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
                },

                0b111 => {
                    if hi & 0xF0 == 0xF0 {
                        arm_op_swi
                    } else {
                        if lo & 1 == 0 {
                            arm_op_cdp
                        } else {
                            if hi & 1 == 0 {
                                arm_op_mrc
                            } else {
                                arm_op_mcr
                            }
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    tbl
}

fn arm_op_bx<C: Context>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    // TODO: 2S + 1N cycles
    assert_eq!(instr & 0x0ffffff0, 0x012fff10);
    let rn = (instr & 0xF) as usize;
    let new_pc = cpu.regs.r[rn];
    cpu.regs.state = new_pc & 1 != 0;
    cpu.set_pc(new_pc & !1);
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

    let rm = cpu.regs.r[(instr & 0xF) as usize];
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

fn decode_reg_sft(cpu: &mut Cpu<impl Context>, instr: u32) -> (u32, bool) {
    if instr & 0x10 == 0 {
        decode_reg_sft_imm(cpu, instr)
    } else {
        decode_reg_sft_reg(cpu, instr)
    }
}

fn decode_op2<C: Context, const I: bool>(cpu: &mut Cpu<C>, instr: u32) -> (u32, bool) {
    if !I {
        decode_reg_sft(cpu, instr)
    } else {
        let imm = instr & 0xFF;
        let rot = (instr >> 8) & 0xF;
        // FIXME: flags?
        (imm.rotate_right(rot * 2), false)
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

    (ret, !(carry1 || carry2), !(overflow1 || overflow2))
}

fn alu<C: Context, const O: u8>(
    cpu: &mut Cpu<C>,
    op1: u32,
    op2: u32,
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
                (
                    op1.wrapping_add(op2).wrapping_add(cpu.regs.c_flag as u32),
                    false,
                )
            } else {
                let (ret, carry, overflow) = add_with_flag(op1, op2, cpu.regs.c_flag);
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
                let (ret, carry, overflow) = sub_with_flag(op1, op2, cpu.regs.c_flag);
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
                let (ret, carry, overflow) = sub_with_flag(op2, op1, cpu.regs.c_flag);
                cpu.regs.c_flag = carry;
                cpu.regs.v_flag = overflow;
                (ret, false)
            }
        }
        // TST
        0b1000 => {
            assert!(change_flag);
            (op1 & op2, true)
        }
        // TEQ
        0b1001 => {
            assert!(change_flag);
            (op1 ^ op2, true)
        }
        // CMP
        0b1010 => {
            assert!(change_flag);
            let (ret, carry, overflow) = sub_with_flag(op1, op2, true);
            cpu.regs.c_flag = carry;
            cpu.regs.v_flag = overflow;
            (ret, true)
        }
        // CMN
        0b1011 => {
            assert!(change_flag);
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
    let rn = ((instr >> 16) & 0xf) as usize;
    let rd = ((instr >> 12) & 0xf) as usize;

    let change_flag = S && rd != 15;

    let op1 = cpu.regs.r[rn];

    let (op2, carry) = decode_op2::<C, I>(cpu, instr);
    if change_flag {
        cpu.regs.c_flag = carry;
    }

    if let Some(res) = alu::<C, O>(cpu, op1, op2, change_flag) {
        if rd != 15 {
            cpu.regs.r[rd] = res;
        } else {
            cpu.set_pc(res);
            if S {
                assert!(cpu.regs.mode != MODE_USER);
                cpu.regs.set_cpsr(cpu.regs.spsr);
            }
        }
    }
}

fn arm_op_mrs<C: Context, const S: bool>(cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    assert_eq!(instr & 0x0FBF0FFF, 0x010F0000);
    let rd = ((instr >> 12) & 0xf) as usize;
    assert_ne!(rd, 15);
    if !S {
        cpu.regs.r[rd] = cpu.regs.spsr;
    } else {
        cpu.regs.r[rd] = cpu.regs.cpsr();
    }
}

fn arm_op_msr<C: Context, const I: bool, const S: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u32,
) {
    let flg = (instr >> 16) & 1;
    if !I {
        assert!(instr & 0x0FBEFFF0 == 0x0128F000);
    } else {
        assert!(instr & 0x0FBEF000 == 0x0328F000);
    }
    let src = decode_op2::<C, I>(cpu, instr).0;
    if !S {
        if flg == 0 {
            cpu.regs.spsr = src;
        } else {
            cpu.regs.spsr = cpu.regs.spsr & !0xF0000000 | src & 0xF0000000;
        }
    } else {
        if flg == 0 {
            cpu.regs.set_cpsr(src);
        } else {
            cpu.regs
                .set_cpsr(cpu.regs.cpsr() & !0xF0000000 | src & 0xF0000000);
        }
    }
}

fn arm_op_mul<C: Context, const A: bool, const S: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u32,
) {
    // TODO: cycles

    let rd = ((instr >> 16) & 0xf) as usize;
    let rn = ((instr >> 12) & 0xf) as usize;
    let rs = ((instr >> 8) & 0xf) as usize;
    let rm = (instr & 0xf) as usize;

    // The destination register Rd must not be the same as the operand register Rm.
    // R15 must not be used as an operand or as the destination register.

    assert_ne!(rd, rm);
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
    if !S {
        cpu.regs.set_nz(res);
    }
}

fn arm_op_mull<C: Context, const U: bool, const A: bool, const S: bool>(
    cpu: &mut Cpu<C>,
    _ctx: &mut C,
    instr: u32,
) {
    let rdhi = ((instr >> 16) & 0xf) as usize;
    let rdlo = ((instr >> 12) & 0xf) as usize;
    let rs = ((instr >> 8) & 0xf) as usize;
    let rm = (instr & 0xf) as usize;

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
        cpu.regs.n_flag = res & 0x80000000 != 0;
        cpu.regs.z_flag = res == 0;
    }
}

trait Data {
    fn load(ctx: &mut impl Context, addr: u32) -> u32;
    fn store(_ctx: &mut impl Context, _addr: u32, _data: u32) {
        panic!()
    }
}

impl Data for u32 {
    fn load(ctx: &mut impl Context, addr: u32) -> u32 {
        let ofs = addr & 3;
        let data = ctx.read32(addr & !3);
        data.rotate_right(ofs * 8)
    }

    fn store(ctx: &mut impl Context, addr: u32, data: u32) {
        ctx.write32(addr & !3, data)
    }
}

impl Data for u16 {
    fn load(ctx: &mut impl Context, addr: u32) -> u32 {
        // unaligned address cause an unpredictable value
        ctx.read16(addr & !1) as u32
    }

    fn store(ctx: &mut impl Context, addr: u32, data: u32) {
        ctx.write16(addr & !1, data as u16)
    }
}

impl Data for i16 {
    fn load(ctx: &mut impl Context, addr: u32) -> u32 {
        ctx.read8(addr & !1) as i16 as u32
    }
}

impl Data for u8 {
    fn load(ctx: &mut impl Context, addr: u32) -> u32 {
        ctx.read8(addr) as u32
    }

    fn store(ctx: &mut impl Context, addr: u32, data: u32) {
        ctx.write8(addr, data as u8)
    }
}

impl Data for i8 {
    fn load(ctx: &mut impl Context, addr: u32) -> u32 {
        ctx.read8(addr) as i8 as u32
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

    let rn = ((instr >> 16) & 0xf) as usize;
    let rd = ((instr >> 12) & 0xf) as usize;

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
            cpu.regs.r[rd] = u32::load(ctx, addr);
        } else {
            cpu.regs.r[rd] = u8::load(ctx, addr);
        };
    } else {
        // When R15 is the source register (Rd) of a register store (STR) instruction,
        // the stored value will be address of the instruction plus 12.
        let data = cpu.regs.r[rd].wrapping_add(if rd == 15 { 4 } else { 0 });
        if B {
            u32::store(ctx, addr, data);
        } else {
            u8::store(ctx, addr, data);
        }
    }

    if W || !P {
        // Write-back must not be specified if R15 is specified as the base register (Rn).
        assert_ne!(rn, 15);
        cpu.regs.r[rn] = ea;
    }
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

    let rn = ((instr >> 16) & 0xf) as usize;
    let rd = ((instr >> 12) & 0xf) as usize;

    let base = cpu.regs.r[rn];

    let offset = if !I {
        // R15 must not be specified as the register offset (Rm).
        let rm = (instr & 0xf) as usize;
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
        cpu.regs.r[rd] = T::load(ctx, addr);
    } else {
        // When R15 is the source register (Rd) of a register store (STR) instruction,
        // the stored value will be address of the instruction plus 12.
        let data = cpu.regs.r[rd].wrapping_add(if rd == 15 { 4 } else { 0 });
        T::store(ctx, addr, data);
    }

    if W || !P {
        // Write-back should not be specified if R15 is specified as the base register (Rn).
        assert_ne!(rn, 15);
        cpu.regs.r[rn] = ea;
    }
}

fn arm_op_ldstm<
    C: Context,
    const P: bool,
    const U: bool,
    const S: bool,
    const W: bool,
    const L: bool,
>(
    cpu: &mut Cpu<C>,
    ctx: &mut C,
    instr: u32,
) {
    // LDM: nS + 1N + 1I cycles (LDM PC: (n+1)S + 2N + 1I)
    // STM: (n-1)S + 2N cycles

    let rn = ((instr >> 16) & 0xf) as usize;

    // R15 should not be used as the base register in any LDM or STM instruction.
    assert_ne!(rn, 15);

    let base = cpu.regs.r[rn];
    let delta = if U { 4 } else { -4_i32 as u32 };

    let mut addr = base;

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

    for i in 0..16 {
        if instr & (1 << i) == 0 {
            continue;
        }

        if P {
            addr = addr.wrapping_add(delta);
        }

        if L {
            let data = u32::load(ctx, addr);
            if i != 15 {
                cpu.regs.r[i] = data;
            } else {
                cpu.set_pc(data);
            }
        } else {
            // Whenever R15 is stored to memory the stored value is the address of the STM
            // instruction plus 12.
            let data = cpu.regs.r[i].wrapping_add(if i == 15 { 4 } else { 0 });
            u32::store(ctx, addr, data);
        }

        if !P {
            addr = addr.wrapping_add(delta);
        }
    }

    if user_bank {
        cpu.regs.change_mode(cur_mode);
    }

    if W {
        assert!(!user_bank);
        cpu.regs.r[rn] = addr;
    }
}

fn arm_op_swp<C: Context, const B: bool>(cpu: &mut Cpu<C>, ctx: &mut C, instr: u32) {
    let rn = ((instr >> 16) & 0xf) as usize;
    let rd = ((instr >> 12) & 0xf) as usize;
    let rm = (instr & 0xf) as usize;

    // Do not use R15 as an operand (Rd, Rn or Rs) in a SWP instruction.
    assert!(rd != 15);
    assert!(rn != 15);
    assert!(rm != 15);

    let addr = cpu.regs.r[rn];
    let src = cpu.regs.r[rm];
    let data = if !B {
        u32::load(ctx, addr)
    } else {
        u8::load(ctx, addr)
    };
    if !B {
        u32::store(ctx, addr, src);
    } else {
        u8::store(ctx, addr, src);
    }
    cpu.regs.r[rd] = data;
}

fn arm_op_swi<C: Context>(cpu: &mut Cpu<C>, _ctx: &mut C, _instr: u32) {
    cpu.exception(Exception::SoftwareInterrupt)
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

fn arm_op_cdp<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, _instr: u32) {
    todo!()
}

fn arm_op_mrc<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, _instr: u32) {
    todo!()
}

fn arm_op_mcr<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, _instr: u32) {
    todo!()
}

fn arm_op_undef<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    trace!("Undefined instruction: {:08x}", instr);
    panic!()
}

fn arm_op_invalid<C: Context>(_cpu: &mut Cpu<C>, _ctx: &mut C, instr: u32) {
    trace!("Invalid instruction: {:08x}", instr);
    panic!()
}

fn disasm(instr: u32, pc: u32) -> String {
    #[rustfmt::skip]
    const COND: [&str; 15] = [
        "EQ", "NE", "CS", "CC", "MI", "PL", "VS", "VC",
        "HI", "LS", "GE", "LT", "GT", "LE", "",
    ];

    let cond = instr >> 28;
    if cond == 15 {
        return "Invalid".to_string();
    }
    let cond = COND[cond as usize];

    let op2 = || -> Option<String> {
        let i = (instr >> 25) & 1;

        if i != 0 {
            let imm = instr & 0xFF;
            let rot = (instr >> 8) & 0xF;
            let opr = imm.rotate_right(rot * 2);
            return if opr < 10 {
                Some(format!("#{opr}"))
            } else {
                Some(format!("#0x{opr:X}"))
            };
        }

        let rm = instr & 0xF;
        let shift_by_reg = (instr >> 4) & 1 != 0;
        let ty = (instr >> 5) & 3;

        const SHIFT_TYPE: [&str; 4] = ["LSL", "LSR", "ASR", "ROR"];
        let ty = SHIFT_TYPE[ty as usize];

        if shift_by_reg {
            if (instr >> 7) & 1 != 0 {
                return None;
            }
            let rs = (instr >> 8) & 0xF;
            return Some(format!("R{rm}, {ty} R{rs}"));
        }

        let amo = (instr >> 7) & 0x1F;
        if amo == 0 {
            Some(format!("R{rm}"))
        } else {
            Some(format!("R{rm}, {ty} #{amo}"))
        }
    };

    let ret = || {
        if instr & 0x0C000000 == 0x00000000 {
            // cccc 00io ooos nnnn dddd opr2 opr2 opr2

            // let i = (instr >> 25) & 1;
            let opc = (instr >> 21) & 0xF;
            let s = (instr >> 20) & 1;

            let op2 = op2();

            if !(opc & 0xC0 == 0x80 && s == 0) && op2.is_some() {
                #[rustfmt::skip]
                const MNE: [&str; 16] = [
                    "AND", "EOR", "SUB", "RSB", "ADD", "ADC", "SBC", "RSC",
                    "TST", "TEQ", "CMP", "CMN", "ORR", "MOV", "BIC", "MVN",
                ];

                let mne = MNE[opc as usize];
                let s = if s != 0 { "S" } else { "" };
                let op2 = op2.unwrap();
                let rn = (instr >> 16) & 0xF;
                let rd = (instr >> 12) & 0xF;

                return match mne {
                    "MOV" | "MVN" => format!("{mne}{cond}{s} R{rd}, {op2}"),
                    "CMP" | "CMN" | "TEQ" | "TST" => format!("{mne} R{rn}, {op2}"),
                    _ => format!("{mne}{cond}{s} R{rd}, R{rn}, {op2}"),
                };
            }

            // compare operations without setting flags
            // are mapped to BX or PSR instructions

            // cccc ..i. .oo. nnnn dddd opr2 opr2 opr2

            if instr & 0x0FFFFFF0 == 0x012FFF10 {
                // cccc 0001 0010 1111 1111 1111 0001 nnnn
                let rn = instr & 0xF;
                return format!("BX{cond} R{rn}");
            }

            let p = (instr >> 22) & 1;
            let psr = if p == 0 { "CPSR" } else { "SPSR" };

            if instr & 0x0FBF0FFF == 0x010F0000 {
                // cccc 0001 0p00 1111 dddd 0000 0000 0000
                let rd = (instr >> 12) & 0xF;
                return format!("MRS{cond} R{rd}, {psr}");
            }

            if instr & 0x0FBFFFF0 == 0x0129F000 {
                // cccc 0001 0p10 1001 1111 0000 0000 mmmm
                let rm = instr & 0xF;
                return format!("MSR{cond} {psr}, R{rm}");
            }

            if instr & 0x0FBFF000 == 0x0128F000 {
                // cccc 0001 0p10 1000 1111 0000 0000 mmmm
                let rm = instr & 0xF;
                return format!("MSR{cond} {psr}_flg, R{rm}");
            }

            if instr & 0x0FBFF000 == 0x0328F000 {
                // cccc 0011 0p10 1000 1111 rrrr iiii iiii
                let rot = (instr >> 8) & 0xF;
                let imm = (instr & 0xFF).rotate_right(rot * 2);
                return format!("MSR{cond} {psr}_flg, #0x{imm:x}");
            }

            if instr & 0x0FC000F0 == 0x00000090 {
                // cccc 0000 00AS dddd nnnn ssss 1001 mmmm
                let rd = (instr >> 16) & 0xF;
                let rn = (instr >> 12) & 0xF;
                let rs = (instr >> 8) & 0xF;
                let rm = instr & 0xF;
                let a = (instr >> 25) & 1;
                let s = (instr >> 24) & 1;
                let s = if s == 0 { "" } else { "S" };

                return if a == 0 {
                    format!("MUL{cond}{s} R{rd}, R{rm}, R{rs}")
                } else {
                    format!("MLA{cond}{s} R{rd}, R{rm}, R{rs}, R{rn}")
                };
            }

            if instr & 0x0FC000F0 == 0x00000090 {
                // cccc 0000 1UAS hhhh llll ssss 1001 mmmm
                let rdhi = (instr >> 16) & 0xF;
                let rdlo = (instr >> 12) & 0xF;
                let rs = (instr >> 8) & 0xF;
                let rm = instr & 0xF;
                let u = (instr >> 26) & 1;
                let u = if u == 0 { "S" } else { "U" };
                let a = (instr >> 25) & 1;
                let s = (instr >> 24) & 1;
                let s = if s == 0 { "" } else { "S" };

                assert_ne!(rdhi, rdlo);
                assert_ne!(rdhi, rm);
                assert_ne!(rdlo, rm);
                assert_ne!(rdhi, 15);
                assert_ne!(rdlo, 15);

                return if a == 0 {
                    format!("{u}MUL{cond}L{s} R{rdlo}, R{rdhi}, R{rm}, R{rs}")
                } else {
                    format!("{u}MLA{cond}L{s} R{rdlo}, R{rdhi}, R{rm}, R{rs}")
                };
            }
        }

        if instr & 0x0C000000 == 0x04000000 {
            let i = (instr >> 25) & 1;
            let pre = (instr >> 24) & 1;
            let up = (instr >> 23) & 1;
            let b = (instr >> 22) & 1;
            let wb = (instr >> 21) & 1;
            let ld = (instr >> 20) & 1;
            let rn = (instr >> 16) & 0xF;
            let rd = (instr >> 12) & 0xF;

            let b = if b == 0 { "" } else { "B" };
            let t = if pre == 0 && wb == 1 { "T" } else { "" };

            let mne = if ld == 0 { "STR" } else { "LDR" };

            let sign = if up == 0 { "-" } else { "+" };

            let addr = {
                let w = if wb == 0 { "" } else { "!" };
                if i == 0 {
                    let ofs = instr & 0xFFF;
                    if ofs == 0 {
                        assert_eq!(w, "");
                        assert_eq!(pre, 1);
                        format!("[R{rn}]")
                    } else {
                        if pre == 0 {
                            format!("[R{rn}, #{sign}{ofs}]{w}")
                        } else {
                            format!("[R{rn}], #{sign}{ofs}")
                        }
                    }
                } else {
                    let rm = instr & 0xF;
                    let shift_by_reg = (instr >> 4) & 1 != 0;

                    assert!(!shift_by_reg);

                    let ty = (instr >> 5) & 3;

                    const SHIFT_TYPE: [&str; 4] = ["LSL", "LSR", "ASR", "ROR"];
                    let ty = SHIFT_TYPE[ty as usize];

                    let amo = (instr >> 7) & 0x1F;
                    if amo == 0 {
                        if pre == 0 {
                            format!("[R{rn}, #{sign}R{rm}]{w}")
                        } else {
                            format!("[R{rn}], #{sign}R{rm}")
                        }
                    } else {
                        if pre == 0 {
                            format!("[R{rn}, #{sign}R{rm}, {ty} #{amo}]{w}")
                        } else {
                            format!("[R{rn}], #{sign}R{rm}, {ty} #{amo}")
                        }
                    }
                }
            };

            return format!("{mne}{cond}{b}{t} R{rd}, {addr}");
        }

        if instr & 0x0E400F90 == 0x00000090 || instr & 0x0E400090 == 0x00400090 {
            // cccc 000P U0WL nnnn dddd 0000 1SH1 mmmm
            // cccc 000P U1WL nnnn dddd oooo 1SH1 oooo

            let sh = (instr >> 5) & 3;
            if sh != 0 {
                let pre = (instr >> 24) & 1;
                let up = (instr >> 23) & 1;
                let i = (instr >> 22) & 1;
                let wb = (instr >> 21) & 1;
                let ld = (instr >> 20) & 1;

                let rn = (instr >> 16) & 0xF;
                let rd = (instr >> 12) & 0xF;

                let ty = match sh {
                    1 => "H",
                    2 => "SB",
                    3 => "SH",
                    _ => unreachable!(),
                };

                let mne = if ld == 0 { "STR" } else { "LDR" };
                let w = if wb == 0 { "" } else { "!" };
                let sign = if up == 0 { "-" } else { "+" };

                let addr = if i == 0 {
                    let rm = instr & 0xF;
                    if pre == 1 {
                        format!("[R{rn}, {sign}R{rm}]{w}")
                    } else {
                        format!("[R{rn}], {sign}R{rm}")
                    }
                } else {
                    let ofs = (instr & 0xF) | ((instr >> 8) & 0xF << 4);

                    if pre == 1 {
                        if ofs == 0 {
                            assert!(w == "");
                            format!("[R{rn}]")
                        } else {
                            format!("[R{rn}, {sign}#0x{ofs}]{w}")
                        }
                    } else {
                        format!("[R{rn}], {sign}#0x{ofs}")
                    }
                };

                return format!("{mne}{cond}{ty} R{rd}, {addr}");
            }
        }

        if instr & 0x0E000000 == 0x08000000 {
            // cccc 100P USWL nnnn regi ster list ....
            let pre = (instr >> 24) & 1;
            let up = (instr >> 23) & 1;
            let s = (instr >> 22) & 1;
            let wb = (instr >> 21) & 1;
            let ld = (instr >> 20) & 1;
            let rn = (instr >> 16) & 0xF;

            let mne = if ld == 0 { "STM" } else { "LDM" };

            let suf = match (ld, pre, up) {
                (1, 1, 1) => "ED",
                (1, 0, 1) => "FD",
                (1, 1, 0) => "EA",
                (1, 0, 0) => "FA",
                (0, 1, 1) => "FA",
                (0, 0, 1) => "EA",
                (0, 1, 0) => "FD",
                (0, 0, 0) => "ED",
                _ => unreachable!(),
            };

            let wb = if wb == 0 { "" } else { "!" };

            let rlist = fmt_reglist(instr as u16);
            let cpsr = if s == 0 { "" } else { "^" };

            return format!("{mne}{cond}{suf} R{rn}{wb}, {rlist}{cpsr}");
        }

        if instr & 0x0FB00FF0 == 0x01000090 {
            // cccc 0001 0B00 nnnn dddd 0000 1001 mmmm
            let b = (instr >> 22) & 1;
            let rn = (instr >> 16) & 0xF;
            let rd = (instr >> 12) & 0xF;
            let rm = instr & 0xF;

            let b = if b == 0 { "" } else { "B" };

            return format!("SWP{cond}{b} R{rd}, R{rm}, [R{rn}]");
        }

        if instr & 0x0E000000 == 0x0A000000 {
            // cccc 101l oooo oooo oooo oooo oooo oooo
            let ofs = ((((instr & 0xFFFFFF) << 8) as i32 >> 8) << 2) as u32;
            let dest = pc.wrapping_add(8).wrapping_add(ofs);
            let l = if (instr & 0x01000000) != 0 { "L" } else { "" };
            return format!("B{l}{cond} 0x{dest:08X}");
        }

        if instr & 0x0F000000 == 0x0F000000 {
            // cccc 1111 .... .... .... .... .... ....
            let comment = instr & 0xFFFFFF;
            return format!("SWI{cond} #0x{comment:06X}");
        }

        if instr & 0x0F000010 == 0x0E000000 {
            // cccc 1110 oooo nnnn dddd #### iii0 mmmm
            let cp_opc = (instr >> 20) & 0xF;
            let crn = (instr >> 16) & 0xF;
            let crd = (instr >> 12) & 0xF;
            let cp_num = (instr >> 8) & 0xF;
            let cp = (instr >> 5) & 0x7;
            let crm = instr & 0xF;

            let expr2 = if cp == 0 {
                "".to_string()
            } else {
                format!(", {cp}")
            };

            return format!("CDP{cond} p{cp_num}, {cp_opc}, c{crd}, c{crn}, c{crm}{expr2}");
        }

        if instr & 0x0F000010 == 0x0E000010 {
            // cccc 1110 oooL nnnn dddd #### iii1 mmmm
            let cp_opc = (instr >> 21) & 0x7;
            let ld = (instr >> 20) & 1;
            let crn = (instr >> 16) & 0xF;
            let rd = (instr >> 12) & 0xF;
            let cp_num = (instr >> 8) & 0xF;
            let cp = (instr >> 5) & 0x7;
            let crm = instr & 0xF;

            let mne = if ld == 0 { "MCR" } else { "MRC" };

            let expr2 = if cp == 0 {
                "".to_string()
            } else {
                format!(", {cp}")
            };

            return format!("{mne}{cond} p{cp_num}, {cp_opc}, R{rd}, c{crn}, c{crm}{expr2}");
        }

        if instr & 0x0E000000 == 0x0C000000 {
            // cccc 110P UNWL nnnn dddd #### oooo oooo
            let pre = (instr >> 24) & 1;
            let up = (instr >> 23) & 1;
            let len = (instr >> 22) & 1;
            let wb = (instr >> 21) & 1;
            let ld = (instr >> 20) & 1;
            let rn = (instr >> 16) & 0xF;
            let crd = (instr >> 12) & 0xF;
            let cp_num = (instr >> 8) & 0xF;
            let offset = instr & 0xFF;

            let mne = if ld == 0 { "STC" } else { "LDC" };
            let l = if len == 0 { "" } else { "L" };
            let sign = if up == 0 { "-" } else { "+" };
            let wb = if wb == 0 { "" } else { "!" };

            let addr = if pre == 1 {
                if offset == 0 {
                    format!("[R{rn}]")
                } else {
                    format!("[R{rn}, {sign}#0x{offset:02X}]{wb}")
                }
            } else {
                format!("[R{rn}], {sign}#0x{offset:02X}")
            };

            return format!("{mne}{cond}{l} p{cp_num}, c{crd}, {addr}");
        }

        if instr & 0x0E000010 == 0x06000010 {
            return format!("UNDEF");
        }

        panic!("Invalid instruction: 0x{:08X}", instr);
    };

    ret()
}

fn fmt_reglist(reglist: u16) -> String {
    use std::fmt::Write;

    assert_ne!(reglist, 0);
    let mut ret = String::new();
    let reglist = reglist.view_bits::<Lsb0>();

    write!(&mut ret, "{{").unwrap();

    for i in 0..16 {
        if !(reglist[i] && (i == 0 || !reglist[i - 1])) {
            continue;
        }

        let mut j = i;
        while j + 1 < 16 && reglist[j + 1] {
            j += 1;
        }

        if i == j {
            write!(&mut ret, "R{i}").unwrap();
        } else if j - i == 1 {
            write!(&mut ret, "R{i}, R{j}").unwrap();
        } else {
            write!(&mut ret, "R{i}-R{j}").unwrap();
        }
    }

    write!(&mut ret, "}}").unwrap();

    ret
}
