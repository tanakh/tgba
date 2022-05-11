use bitvec::prelude::*;
use log::{info, trace};

use crate::{
    consts::{CLOCK_PER_DOT, SCREEN_HEIGHT, SCREEN_WIDTH, VISIBLE_HEIGHT, VISIBLE_WIDTH},
    context::{Interrupt, Timing},
    interrupt::InterruptKind,
    util::trait_alias,
};

#[derive(Default)]
pub struct Lcd {
    pub vram: Vec<u8>,
    pub oam: Vec<u8>,
    pub palette: Vec<u8>,

    bg_mode: u8,
    display_frame_select: bool,
    hblank_obj_process: bool,
    obj_format: bool,
    force_blank: bool,
    display_bg: [bool; 4],
    display_obj: bool,
    display_window: [bool; 2],
    display_obj_window: bool,

    vblank_status: bool,
    hblank_status: bool,
    vcounter_status: bool,
    vblank_irq_enable: bool,
    hblank_irq_enable: bool,
    vcount_irq_enable: bool,
    vcount_irq: u8,

    bg: [Bg; 4],
    window: [Window; 2],
    winin: [WindowCtrl; 2],
    winout: WindowCtrl,
    objwin: WindowCtrl,

    bg_mosaic_h: u8,
    bg_mosaic_v: u8,
    obj_mosaic_h: u8,
    obj_mosaic_v: u8,

    blend_ctrl: BlendCtrl,

    prev_clock: u64,
    fraction: u64,
    x: u32,
    y: u32,
    frame: u64,
}

#[derive(Default)]
struct Bg {
    priority: u8,
    char_base_block: u8,
    mosaic: bool,
    color_mode: bool, // 0: 16 colors x 16 palettes, 1: 256 colors x 1 palette
    screen_base_block: u8,
    area_overflow_processing: bool, // 0: transparent, 1: wraparound
    screen_size: u8,

    hofs: u16,
    vofs: u16,

    dx: u16,
    dmx: u16,
    dy: u16,
    dmy: u16,

    x: u32,
    y: u32,
}

#[derive(Default)]
struct Window {
    l: u8,
    r: u8,
    u: u8,
    d: u8,
}

#[derive(Default)]
struct WindowCtrl {
    display_bg: [bool; 4],
    display_obj: bool,
    color_special_effect: bool,
}

#[derive(Default)]
struct BlendCtrl {
    // 0b00: No special effects
    // 0b01: Alpha blending
    // 0b10: Brightness increase
    // 0b11: Brightness decrease
    effect: u8,
    target: [u8; 2],
    eva: u8,
    evb: u8,
    evy: u8,
}

trait_alias!(pub trait Context = Timing + Interrupt);

impl Lcd {
    pub fn new() -> Lcd {
        Lcd {
            vram: vec![0; 96 * 1024],
            oam: vec![0; 1024],
            palette: vec![0; 1024],
            ..Default::default()
        }
    }

    pub fn frame(&self) -> u64 {
        self.frame
    }

    pub fn tick(&mut self, ctx: &mut impl Context) {
        let now = ctx.now();
        let elapsed = now - self.prev_clock;
        self.prev_clock = now;

        self.fraction += elapsed;

        while self.fraction >= CLOCK_PER_DOT {
            self.fraction -= CLOCK_PER_DOT;
            self.tick_dot(ctx);
        }
    }

    fn tick_dot(&mut self, ctx: &mut impl Context) {
        self.x += 1;

        if self.y < VISIBLE_HEIGHT && self.x == VISIBLE_WIDTH {
            // TODO: HBLANK
            info!("Enter HBLANK: frame:{}, y:{:03}", self.frame, self.y);

            if self.hblank_irq_enable {
                ctx.interrupt_mut().set_interrupt(InterruptKind::HBlank);
            }
        }

        if self.x >= SCREEN_WIDTH {
            self.x -= SCREEN_WIDTH;
            self.y += 1;

            trace!("Frame:{}, Line:{:03}", self.frame, self.y);

            if self.y as u32 == VISIBLE_HEIGHT {
                // TODO: VBLANK
                info!("Enter VBLANK: frame:{}", self.frame);

                if self.vblank_irq_enable {
                    ctx.interrupt_mut().set_interrupt(InterruptKind::VBlank);
                }

                // TODO: render line
            }

            if self.y == self.vcount_irq as u32 && self.vcount_irq_enable {
                if self.vblank_irq_enable {
                    ctx.interrupt_mut().set_interrupt(InterruptKind::VCount);
                }
            }

            if self.y >= SCREEN_HEIGHT {
                self.y -= SCREEN_HEIGHT;
                self.frame += 1;
            }
        }
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        match addr {
            // VCOUNT
            0x006 => self.y as u16,

            _ => todo!("LCD read {addr:03X}"),
        }
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        match addr {
            // DISPCNT
            0x000 => {
                let v = data.view_bits::<Lsb0>();
                self.bg_mode = v[0..=2].load();
                self.display_frame_select = v[4];
                self.hblank_obj_process = v[5];
                self.obj_format = v[6];
                self.force_blank = v[7];
                self.display_bg[0] = v[8];
                self.display_bg[1] = v[9];
                self.display_bg[2] = v[10];
                self.display_bg[3] = v[11];
                self.display_obj = v[12];
                self.display_window[0] = v[13];
                self.display_window[1] = v[14];
                self.display_obj_window = v[15];
            }

            // DISPSTAT
            0x004 => {
                let v = data.view_bits::<Lsb0>();
                self.vblank_irq_enable = v[3];
                self.hblank_irq_enable = v[4];
                self.vcount_irq_enable = v[5];
                self.vcount_irq = v[8..=15].load();
            }

            // VCOUNT
            0x006 => {}

            // BGxCnt
            0x008 | 0x00A | 0x00C | 0x00E => {
                let i = ((addr - 0x008) / 2) as usize;
                let bg_cntl = &mut self.bg[i];
                let v = data.view_bits::<Lsb0>();
                bg_cntl.priority = v[0..=1].load();
                bg_cntl.char_base_block = v[2..=3].load();
                bg_cntl.mosaic = v[6];
                bg_cntl.color_mode = v[7];
                bg_cntl.screen_base_block = v[8..=12].load();
                if i == 2 || i == 3 {
                    bg_cntl.area_overflow_processing = v[13];
                }
                bg_cntl.screen_size = v[14..=15].load();
            }

            // BGxHOFS
            0x010 | 0x014 | 0x018 | 0x01C => {
                let i = ((addr - 0x010) / 4) as usize;
                self.bg[i].hofs = data & 0x1FF;
            }

            // BGxVOFS
            0x012 | 0x016 | 0x01A | 0x01E => {
                let i = ((addr - 0x012) / 4) as usize;
                self.bg[i].vofs = data & 0x1FF;
            }

            // BGxPA
            0x020 | 0x030 => {
                let i = (2 + (addr - 0x020) / 0x10) as usize;
                self.bg[i].dx = data;
            }
            // BGxPB
            0x022 | 0x032 => {
                let i = (2 + (addr - 0x022) / 0x10) as usize;
                self.bg[i].dmx = data;
            }
            // BGxPC
            0x024 | 0x034 => {
                let i = (2 + (addr - 0x024) / 0x10) as usize;
                self.bg[i].dy = data;
            }
            // BGxPD
            0x026 | 0x036 => {
                let i = (2 + (addr - 0x026) / 0x10) as usize;
                self.bg[i].dmy = data;
            }

            // BGxX
            0x028 | 0x038 => {
                let i = (2 + (addr - 0x028) / 0x10) as usize;
                self.bg[i].x.view_bits_mut::<Lsb0>()[0..=15].store(data);
            }
            0x02A | 0x03A => {
                let i = (2 + (addr - 0x028) / 0x10) as usize;
                self.bg[i].x.view_bits_mut::<Lsb0>()[16..=28].store(data);
            }
            // BGxY
            0x02C | 0x03C => {
                let i = (2 + (addr - 0x028) / 0x10) as usize;
                self.bg[i].y.view_bits_mut::<Lsb0>()[0..=15].store(data);
            }
            0x02E | 0x03E => {
                let i = (2 + (addr - 0x028) / 0x10) as usize;
                self.bg[i].y.view_bits_mut::<Lsb0>()[16..=28].store(data);
            }

            // WINxH
            0x040 | 0x042 => {
                let i = ((addr - 0x040) / 2) as usize;
                self.window[i].l = (data >> 8) as u8;
                self.window[i].r = data as u8;
            }
            // WINxV
            0x044 | 0x046 => {
                let i = ((addr - 0x044) / 2) as usize;
                self.window[i].u = (data >> 8) as u8;
                self.window[i].d = data as u8;
            }

            // WININ / WINOUT
            0x048 | 0x04A => {
                let v = data.view_bits::<Lsb0>();
                for i in 0..2 {
                    let ctrl = if addr == 0x048 {
                        &mut self.winin[i]
                    } else {
                        if i == 0 {
                            &mut self.winout
                        } else {
                            &mut self.objwin
                        }
                    };

                    for j in 0..4 {
                        ctrl.display_bg[j] = v[i * 8 + j];
                    }
                    ctrl.display_obj = v[i * 8 + 4];
                    ctrl.color_special_effect = v[i * 8 + 5];
                }
            }

            // MOSAIC
            0x04C => {
                let v = data.view_bits::<Lsb0>();
                self.bg_mosaic_h = v[0..=3].load();
                self.bg_mosaic_v = v[4..=7].load();
                self.obj_mosaic_h = v[8..=11].load();
                self.obj_mosaic_v = v[12..=15].load();
            }
            0x04E => {}

            // BLDCNT
            0x050 => {
                let v = data.view_bits::<Lsb0>();
                self.blend_ctrl.effect = v[6..=7].load();
                self.blend_ctrl.target[0] = v[0..=5].load();
                self.blend_ctrl.target[1] = v[8..=13].load();
            }
            // BLDALPHA
            0x052 => {
                let v = data.view_bits::<Lsb0>();
                self.blend_ctrl.eva = v[0..=4].load();
                self.blend_ctrl.evb = v[8..=12].load();
            }
            // BLDY
            0x054 => {
                let v = data.view_bits::<Lsb0>();
                self.blend_ctrl.evy = v[0..=4].load();
            }

            0x056..=0x05E => {}

            _ => todo!(),
        }
    }
}
