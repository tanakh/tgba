use log::debug;

use crate::{
    context::{Bus, Interrupt, Timing},
    cpu::Cpu,
    util::trait_alias,
};

trait_alias!(pub trait Context = Bus + Timing + Interrupt);

struct BiosFunctionInfo {
    id: u8,
    name: &'static str,
    args: &'static [BiosFunctionArg],
    // ret,
}

struct BiosFunctionArg {
    name: &'static str,
    ty: &'static str,
}

macro_rules! bios_functions {
    ($id:literal $name:ident ($($arg_name:ident : $arg_type:ty),*) $($rest:tt)*) => {
        bios_functions!($($rest)* (BiosFunctionInfo {
                id: $id,
                name: stringify!($name),
                args: &[$(BiosFunctionArg {
                    name: stringify!($arg_name),
                    ty: stringify!($arg_type),
                }),*],
        }))
    };

    ($($entry:tt)*) => {
        &[ $($entry),* ]
    };
}

const BIOS_FUNCTIONS: &[BiosFunctionInfo] = bios_functions! {
    0x00 SoftReset()
    0x01 RegisterRamReset(reset_flags: u8)
    0x02 Halt()
    0x03 StopSleep()
    0x04 IntrWait(force_wait_next: bool, irq_to_wait: u32)
    0x05 VBlankIntrWait()
    0x06 Div(num: i32, denom: i32)
    0x07 DivArm(num: i32, denom: i32)
    0x08 Sqrt(num: u32)
    0x09 ArcTan(tan: f_1_14)
    0x0A ArcTan2(x: f_1_14, y: f_1_14)
    0x0B CpuSet(src: addr, dest: addr, __: length_mode)
    0x0C CpuFastSet(src: addr, dest: addr, __: length_mode)
    0x0D GetBiosChecksum()
    0x0E BgAffineSet(src_info: addr, dest_info: addr)
    0x0F ObjAffineSet(src_info: addr, dest_info: addr)
    0x10 BitUnPack(src: addr, dest: addr, unpack_info: addr)
    0x11 LZ77UnCompReadNormalWrite8bit(src_info: addr, dest: addr)
    0x12 LZ77UnCompReadNormalWrite16bit(src_info: addr, dest: addr)
    0x13 HuffUnCompReadNormal(src_info: addr, dest: addr)
    0x14 RLUnCompReadNormalWrite8bit(src_info: addr, dest: addr)
    0x15 RLUnCompReadNormalWrite16bit(src_info: addr, dest: addr)
    0x16 Diff8bitUnFilterWrite8bit(src_info: addr, dest: addr)
    0x17 Diff8bitUnFilterWrite16bit(src_info: addr, dest: addr)
    0x18 Diff16bitUnFilter(src_info: addr, dest: addr)
    0x19 SoundBias(bias_level: u16)
    0x1A SoundDriverInit(work: addr)
    0x1B SoundDriverMode(mode: u32)
    0x1C SoundDriverMain()
    0x1D SoundDriverVSync()
    0x1E SoundChannelClear()
    0x1F MidiKey2Freq(wave_data: addr, mk: u8, fp: u8)
    0x20 SoundWhatever0()
    0x21 SoundWhatever1()
    0x22 SoundWhatever2()
    0x23 SoundWhatever3()
    0x24 SoundWhatever4()
    0x25 MultiBoot(param: addr, trans_mode: u8)
    0x26 HardReset()
    0x27 CustomHalt(arg1: u32, arg2: u32, halt_or_stop: u8)
    0x28 SoundDriverVSyncOff()
    0x29 SoundDriverVSyncOn()
    0x2A SoundGetJumpList(dest: addr)
};

const UNKNOWN_BIOS_FUNCTION: BiosFunctionInfo = BiosFunctionInfo {
    id: 0xFF,
    name: "Unknown",
    args: &[],
};

pub fn trace_swi<C: Context>(cpu: &mut Cpu<C>, id: u8, pc: u32) {
    if !log::log_enabled!(log::Level::Debug) {
        return;
    }

    let info = BIOS_FUNCTIONS
        .iter()
        .find(|i| i.id == id)
        .unwrap_or(&UNKNOWN_BIOS_FUNCTION);

    let extract_arg = |regno: usize, arg: &BiosFunctionArg| {
        let name = arg.name;
        let val = cpu.regs().r(regno);
        match arg.ty {
            "bool" => {
                if val == 0 || val == 1 {
                    format!("{name}: {}", val != 0)
                } else {
                    format!("{name}: {} (={val:08X})", val != 0)
                }
            }
            "u8" => format!("{name}: 0x{val:02X}"),
            "u16" => format!("{name}: 0x{val:04X}"),
            "u32" => format!("{name}: 0x{val:8X}"),
            "i32" => format!("{name}: {}", val as i32),
            "addr" => format!("{name}: 0x{val:08X}"),

            "f_1_14" => format!("{name}: {}", val as u16 as i16 as f64 / 16384.0),

            "length_mode" => {
                let count = val & 0x1FFFFF;
                let mode = if (val >> 24) & 0x1 == 0 {
                    "copy"
                } else {
                    "fill"
                };
                let size = if (val >> 26) & 0x1 == 0 { 16 } else { 32 };
                format!("param: ({count}x{size}bit, {mode})")
            }
            _ => unreachable!("{}", info.name),
        }
    };

    let args = info
        .args
        .iter()
        .enumerate()
        .map(|(i, arg)| extract_arg(i, arg))
        .collect::<Vec<_>>()
        .join(", ");

    debug!("SWI {id:02X}h from 0x{pc:08X}: {}({args})", info.name);
}
