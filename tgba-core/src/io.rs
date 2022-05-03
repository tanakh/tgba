// trace_macros!(true);

use log::trace;

use crate::{context::Interrupt, util::trait_alias};

trait_alias!(pub trait Context = Interrupt);

#[derive(Default)]
pub struct Io {
    interrupt_master_enable: bool,
    interrupt_enable: [bool; 14],
    post_boot: u8,
}

enum InterruptSource {
    VBlank = 0,
    HBlank = 1,
    VCount = 2,
    Timer0 = 3,
    Timer1 = 4,
    Timer2 = 5,
    Timer3 = 6,
    Serial = 7,
    Dma0 = 8,
    Dma1 = 9,
    Dma2 = 10,
    Dma3 = 11,
    Keypad = 12,
    GamePak = 13,
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
    // 0x4000088  2  BIOS SOUNDBIAS   "Sound PWM Control"
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
    0x4000120  2    R/W  SIOMULTI0 "SIO Data 0 (Parent)    (Multi-Player Mode)"
    0x4000122  2    R/W  SIOMULTI1 "SIO Data 1 (1st Child) (Multi-Player Mode)"
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

fn get_io_reg(addr: u32) -> Option<&'static IoReg> {
    IO_REGS.iter().find(|r| r.addr & 0x3FF == addr)
}

fn trace_rw(addr: u32, data: u32, size: usize, read: bool) {
    if !log::log_enabled!(log::Level::Trace) {
        return;
    }

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

    let dir = if read { "Read" } else { "Write" };

    trace!("{dir}{size}: 0x{addr:03X} = 0x{data} # {annot}");
}

impl Io {
    pub fn new() -> Self {
        Io::default()
    }

    pub fn read8(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        let data = match addr {
            // POSTFLG
            0x300 => self.post_boot,
            _ => unreachable!(),
        };

        trace_rw(addr, data as u32, 8, true);
        data
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        todo!()
    }

    pub fn read32(&mut self, ctx: &mut impl Context, addr: u32) -> u32 {
        todo!()
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        trace_rw(addr, data as u32, 8, false);

        match addr {
            // Interrupt Master Enable
            0x208 => self.interrupt_master_enable = (data & 1) != 0,

            // POSTFLG
            0x300 => self.post_boot = data,

            _ => unreachable!(),
        }
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        todo!()
    }

    pub fn write32(&mut self, ctx: &mut impl Context, addr: u32, data: u32) {
        todo!()
    }
}
