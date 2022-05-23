use std::cmp::min;

use bitvec::prelude::*;
use log::{info, trace};

use crate::{
    consts::{CLOCK_PER_DOT, DOTS_PER_LINE, LINES_PER_FRAME, SCREEN_HEIGHT, SCREEN_WIDTH},
    context::{Interrupt, Timing},
    interface::{FrameBuf, Pixel},
    interrupt::InterruptKind,
    util::{pack, trait_alias},
};

trait_alias!(pub trait Context = Timing + Interrupt);

#[derive(Default)]
pub struct Lcd {
    pub vram: Vec<u8>,
    pub oam: Vec<u8>,
    pub palette: Vec<u8>,

    bg_mode: u8,
    display_frame_select: bool,
    hblank_obj_process: bool, // 0: enable, 1: disable
    obj_format: bool,         // 0: 2-dim, 1: 1-dim
    force_blank: bool,
    display_bg: [bool; 4],
    display_obj: bool,
    display_window: [bool; 2],
    display_obj_window: bool,

    vblank_irq_enable: bool,
    hblank_irq_enable: bool,
    vcount_irq_enable: bool,
    vcount: u8,

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

    line_buf: LineBuf,
    frame_buf: FrameBuf,
}

struct LineBuf {
    bg: [Vec<u16>; 4],
    obj: Vec<u16>,
    obj_attr: Vec<ObjAttr>,
    surface: [Vec<u16>; 2],
    surface_priority: [Vec<u8>; 2],
    surface_attr: [Vec<u8>; 2],
    finished: Vec<u16>,
}

impl Default for LineBuf {
    fn default() -> Self {
        Self {
            bg: <[Vec<u16>; 4]>::default().map(|_| vec![0x8000; SCREEN_WIDTH as _]),
            obj: vec![0; SCREEN_WIDTH as _],
            obj_attr: vec![Default::default(); SCREEN_WIDTH as _],
            surface: <[Vec<u16>; 2]>::default().map(|_| vec![0x8000; SCREEN_WIDTH as _]),
            surface_priority: <[Vec<u8>; 2]>::default().map(|_| vec![0; SCREEN_WIDTH as _]),
            surface_attr: <[Vec<u8>; 2]>::default().map(|_| vec![0; SCREEN_WIDTH as _]),
            finished: vec![0; SCREEN_WIDTH as _],
        }
    }
}

impl LineBuf {
    fn clear(&mut self) {
        self.obj_attr.fill(ObjAttr::default());
        self.obj.fill(0x8000);
        for i in 0..4 {
            self.bg[i].fill(0x8000);
        }
        self.surface_priority[0].fill(4);
        self.surface_priority[1].fill(4);
    }
}

#[derive(Default)]
struct Bg {
    priority: u8,
    char_base_block: u8,
    mosaic: bool,
    color_mode: bool, // 0: 16 colors x 16 palettes, 1: 256 colors x 1 palette
    screen_base_block: u8,
    area_overflow: bool, // 0: transparent, 1: wraparound
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

#[derive(Default, Clone, Debug)]
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

#[derive(Default, Clone)]
struct ObjAttr(u8);

impl ObjAttr {
    fn priority(&self) -> u8 {
        self.0 & 0x3
    }
    fn set_priority(&mut self, priority: u8) {
        self.0 = (self.0 & !3) | (priority & 0x3);
    }

    fn semi_transparent(&self) -> bool {
        (self.0 & 4) != 0
    }
    fn set_semi_transparent(&mut self, semi: bool) {
        self.0 = (self.0 & !4) | (semi as u8) << 2;
    }

    fn window(&self) -> bool {
        (self.0 & 8) != 0
    }
    fn set_window(&mut self, window: bool) {
        self.0 = (self.0 & !8) | (window as u8) << 3;
    }
}

impl Lcd {
    pub fn new() -> Lcd {
        Lcd {
            vram: vec![0; 96 * 1024],
            oam: vec![0; 1024],
            palette: vec![0; 1024],
            frame_buf: FrameBuf::new(SCREEN_WIDTH, SCREEN_HEIGHT),
            ..Default::default()
        }
    }

    pub fn frame(&self) -> u64 {
        self.frame
    }

    pub fn line(&self) -> u32 {
        self.y
    }

    pub fn frame_buf(&self) -> &FrameBuf {
        &self.frame_buf
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

        if self.y < SCREEN_HEIGHT && self.x == SCREEN_WIDTH {
            // TODO: HBLANK
            info!("Enter HBLANK: frame:{}, y:{:03}", self.frame, self.y);

            self.render_line();

            if self.hblank_irq_enable {
                ctx.interrupt_mut().set_interrupt(InterruptKind::HBlank);
            }
        }

        if self.x >= DOTS_PER_LINE {
            self.x -= DOTS_PER_LINE;
            self.y += 1;

            trace!("Frame:{}, Line:{:03}", self.frame, self.y);

            if self.y == SCREEN_HEIGHT {
                // TODO: VBLANK
                info!("Enter VBLANK: frame:{}", self.frame);

                if self.vblank_irq_enable {
                    ctx.interrupt_mut().set_interrupt(InterruptKind::VBlank);
                }
            }

            if self.y == self.vcount as u32 && self.vcount_irq_enable {
                if self.vblank_irq_enable {
                    ctx.interrupt_mut().set_interrupt(InterruptKind::VCount);
                }
            }

            if self.y >= LINES_PER_FRAME {
                self.y -= LINES_PER_FRAME;
                self.frame += 1;
            }
        }
    }

    pub fn vblank(&self) -> bool {
        self.y >= SCREEN_HEIGHT
    }

    pub fn hblank(&self) -> bool {
        self.y < SCREEN_HEIGHT && self.x >= SCREEN_WIDTH
    }

    fn vcount_match(&self) -> bool {
        self.y == self.vcount as u32
    }

    pub fn read16(&mut self, _ctx: &mut impl Context, addr: u32) -> u16 {
        match addr {
            // DISPCNT
            0x000 => pack! {
                0..=2 => self.bg_mode,
                4 => self.display_frame_select,
                5 => self.hblank_obj_process,
                6 => self.obj_format,
                7 => self.force_blank,
                8 => self.display_bg[0],
                9 => self.display_bg[1],
                10 => self.display_bg[2],
                11 => self.display_bg[3],
                12 => self.display_obj,
                13 => self.display_window[0],
                14 => self.display_window[1],
                15 => self.display_obj_window,
            },
            0x002 => 0,

            // DISPSTAT
            0x004 => pack! {
                0 => self.vblank(),
                1 => self.hblank(),
                2 => self.vcount_match(),
                3 => self.vblank_irq_enable,
                4 => self.hblank_irq_enable,
                5 => self.vcount_irq_enable,
                8..=15 => self.vcount,
            },

            // VCOUNT
            0x006 => self.y as u16,

            // BGxCNT
            0x008 | 0x00A | 0x00C | 0x00E => {
                let i = ((addr - 0x008) / 2) as usize;
                let bg_ctrl = &mut self.bg[i];
                pack! {
                    0..=1   => bg_ctrl.priority,
                    2..=3   => bg_ctrl.char_base_block,
                    4..=5   => !0,
                    6       => bg_ctrl.mosaic,
                    7       => bg_ctrl.color_mode,
                    8..=12  => bg_ctrl.screen_base_block,
                    13      => bg_ctrl.area_overflow,
                    14..=15 => bg_ctrl.screen_size,
                }
            }

            // BGxHOFS
            0x010 | 0x014 | 0x018 | 0x01C => 0,
            // BGxVOFS
            0x012 | 0x016 | 0x01A | 0x01E => 0,
            // BGxPA
            0x020 | 0x030 => 0,
            // BGxPB
            0x022 | 0x032 => 0,
            // BGxPC
            0x024 | 0x034 => 0,
            // BGxPD
            0x026 | 0x036 => 0,

            // BGxX
            0x028 | 0x038 => 0,
            0x02A | 0x03A => 0,
            // BGxY
            0x02C | 0x03C => 0,
            0x02E | 0x03E => 0,

            // WINxH
            0x040 | 0x042 => 0,
            // WINxV
            0x044 | 0x046 => 0,

            // WININ / WINOUT
            0x048 | 0x04A => {
                let mut data = 0;
                let v = data.view_bits_mut::<Lsb0>();
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
                        v.set(i * 8 + j, ctrl.display_bg[j]);
                    }
                    v.set(i * 8 + 4, ctrl.display_obj);
                    v.set(i * 8 + 5, ctrl.color_special_effect);
                }
                data
            }

            // MOSAIC
            0x04C | 0x04E => 0,

            // BLDCNT
            0x050 => pack! {
                6..=7  => self.blend_ctrl.effect,
                0..=5  => self.blend_ctrl.target[0],
                8..=13 => self.blend_ctrl.target[1],
            },
            // BLDALPHA
            0x052 => pack! {
                0..=4  => self.blend_ctrl.eva,
                8..=12 => self.blend_ctrl.evb,
            },
            // BLDY
            0x054 => 0,

            0x056..=0x05E => 0,

            _ => unreachable!("{addr:03X}"),
        }
    }

    pub fn write16(&mut self, _ctx: &mut impl Context, addr: u32, data: u16) {
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

                eprintln!("Set BG mode: {} at line: {}", self.bg_mode, self.y);
            }
            0x002 => {}

            // DISPSTAT
            0x004 => {
                let v = data.view_bits::<Lsb0>();
                self.vblank_irq_enable = v[3];
                self.hblank_irq_enable = v[4];
                self.vcount_irq_enable = v[5];
                self.vcount = v[8..=15].load();
            }

            // VCOUNT
            0x006 => {}

            // BGxCNT
            0x008 | 0x00A | 0x00C | 0x00E => {
                let i = ((addr - 0x008) / 2) as usize;
                let bg_ctrl = &mut self.bg[i];
                let v = data.view_bits::<Lsb0>();
                bg_ctrl.priority = v[0..=1].load();
                bg_ctrl.char_base_block = v[2..=3].load();
                bg_ctrl.mosaic = v[6];
                bg_ctrl.color_mode = v[7];
                bg_ctrl.screen_base_block = v[8..=12].load();
                if i == 2 || i == 3 {
                    bg_ctrl.area_overflow = v[13];
                }
                bg_ctrl.screen_size = v[14..=15].load();
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
                self.bg[i].x.view_bits_mut::<Lsb0>()[16..=27].store(data);
            }
            // BGxY
            0x02C | 0x03C => {
                let i = (2 + (addr - 0x028) / 0x10) as usize;
                self.bg[i].y.view_bits_mut::<Lsb0>()[0..=15].store(data);
            }
            0x02E | 0x03E => {
                let i = (2 + (addr - 0x028) / 0x10) as usize;
                self.bg[i].y.view_bits_mut::<Lsb0>()[16..=27].store(data);
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

            _ => unreachable!("Invalid read from {addr:03X}"),
        }
    }
}

const OBJ_BASE_ADDR: u32 = 0x10000;

impl Lcd {
    fn render_line(&mut self) {
        if self.force_blank {
            for x in 0..SCREEN_WIDTH {
                *self.frame_buf.pixel_mut(x, self.y) = Pixel::new(255, 255, 255);
            }
            return;
        }

        self.line_buf.clear();

        trace!("Render line: y = {}, mode = {}", self.y, self.bg_mode);

        self.render_obj();

        // eprint!("OBj: ");
        // for x in 0..VISIBLE_WIDTH as usize {
        //     eprint!(
        //         "{:04X}:{:02X} ",
        //         self.line_buf.obj[x], self.line_buf.obj_attr[x].0
        //     );
        // }
        // eprintln!();

        match self.bg_mode {
            0 => {
                self.render_text_bg(0);
                self.render_text_bg(1);
                self.render_text_bg(2);
                self.render_text_bg(3);
            }
            1 => {
                self.render_text_bg(0);
                self.render_text_bg(1);
                self.render_rotate_bg(2);
            }
            2 => {
                self.render_rotate_bg(2);
                self.render_rotate_bg(3);
            }
            3 => self.render_mode3_bg(),
            4 => self.render_mode4_bg(),
            5 => self.render_mode5_bg(),

            _ => panic!("Invalid BG mode: {}", self.bg_mode),
        }

        // for i in 0..4 {
        //     eprint!("BG{i}: ");
        //     for x in 0..VISIBLE_WIDTH as usize {
        //         eprint!("{:04X} ", self.line_buf.bg[i][x]);
        //     }
        //     eprintln!();
        // }

        self.eval_priority();

        // for i in 0..2 {
        //     eprint!("Surface{i}: ");
        //     for x in 0..VISIBLE_WIDTH as usize {
        //         eprint!(
        //             "{x:03}:{:04X}:{}:{} ",
        //             self.line_buf.surface[i][x],
        //             self.line_buf.surface_priority[i][x],
        //             self.line_buf.surface_attr[i][x]
        //         );
        //     }
        //     eprintln!();
        // }

        self.color_special_effect();

        for x in 0..SCREEN_WIDTH {
            *self.frame_buf.pixel_mut(x, self.y) =
                Pixel::from_u16(self.line_buf.finished[x as usize]);
        }
    }

    fn render_text_bg(&mut self, i: usize) {
        if !self.display_bg[i] {
            return;
        }

        let hscrs = (1 + self.bg[i].screen_size % 2) as u32;
        let vscrs = (1 + self.bg[i].screen_size / 2) as u32;

        let screen_base_addr = self.bg[i].screen_base_block as usize * 0x800;
        let char_base_addr = self.bg[i].char_base_block as usize * 0x4000;

        let scry = if self.bg[i].mosaic {
            let mh = self.bg_mosaic_v as u32 + 1;
            self.y / mh * mh
        } else {
            self.y
        };

        let cy = self.bg[i].vofs as u32 + scry;
        let oy = cy % 8;
        let by = cy / 8;

        let scry = by / 32 % vscrs;
        let by = by % 32;

        for x in 0..SCREEN_WIDTH {
            let relx = if self.bg[i].mosaic {
                let mw = self.bg_mosaic_h as u32 + 1;
                x / mw * mw
            } else {
                x
            };

            let cx = self.bg[i].hofs as u32 + relx;
            let ox = cx % 8;
            let bx = cx / 8;

            let scrx = bx / 32 % hscrs;
            let bx = bx % 32;

            let scrid = scry * hscrs + scrx;
            let screen_base_addr = screen_base_addr + scrid as usize * 0x800;
            let block_addr = screen_base_addr + by as usize * 64 + bx as usize * 2;

            let b0 = self.vram[block_addr];
            let b1 = self.vram[block_addr + 1];

            let char = b0 as usize + ((b1 as usize & 3) << 8);
            let hflip = (b1 >> 2) & 1 != 0;
            let vflip = (b1 >> 3) & 1 != 0;
            let palette = b1 >> 4;

            let ox = if !hflip { ox } else { 7 - ox } as usize;
            let oy = if !vflip { oy } else { 7 - oy } as usize;

            if !self.bg[i].color_mode {
                // 16 x 16 color mode
                assert!(char_base_addr + char * 32 + oy * 4 + ox / 2 < self.vram.len(), "too large index: char_base: {char_base_addr:08X}, char: 0x{char:03X}, ox: {ox}, oy: {oy}, b0: 0x{b0:02X}, b1: 0x{b1:02X}");

                let tmp = self.vram[char_base_addr + char * 32 + oy * 4 + ox / 2];
                let col = (tmp >> ((ox & 1) * 4)) & 0xF;
                if col != 0 {
                    self.line_buf.bg[i][x as usize] = self.bg_palette16(palette as _, col as _);
                }
            } else {
                // 256 x 1 color mode
                let col = self.vram[char_base_addr + char * 64 + oy * 8 + ox];
                if col != 0 {
                    self.line_buf.bg[i][x as usize] = self.bg_palette256(col as _);
                }
            };
        }
    }

    fn render_rotate_bg(&mut self, i: usize) {
        if !self.display_bg[i] {
            return;
        }

        const BG_SIZE_TBL: &[u32] = &[128, 256, 512, 1024];

        let size = BG_SIZE_TBL[self.bg[i].screen_size as usize];
        let bw = size as usize / 8;

        let screen_base_addr = self.bg[i].screen_base_block as usize * 0x800;
        let char_base_addr = self.bg[i].char_base_block as usize * 0x4000;

        let (cx, cy) = self.calc_left_for_line(i);

        for x in 0..SCREEN_WIDTH {
            if let Some((rx, ry)) =
                self.calc_refpoint_for_x(i, size, size, self.bg[i].area_overflow, x, cx, cy)
            {
                let bx = (rx / 8) as usize;
                let by = (ry / 8) as usize;

                let ox = (rx % 8) as usize;
                let oy = (ry % 8) as usize;

                let char = self.vram[screen_base_addr + by * bw + bx] as usize;
                let col_num = self.vram[char_base_addr + char * 64 + oy * 8 + ox];

                if col_num != 0 {
                    self.line_buf.bg[i][x as usize] = self.bg_palette256(col_num as _);
                }
            }
        }
    }

    fn render_mode3_bg(&mut self) {
        let i = 2;

        if !self.display_bg[i] {
            return;
        }

        let (cx, cy) = self.calc_left_for_line(i);

        for x in 0..SCREEN_WIDTH {
            if let Some((rx, ry)) = self.calc_refpoint_for_x(i, 240, 160, false, x, cx, cy) {
                let addr = (ry * 240 + rx) as usize * 2;
                let col = u16::from_le_bytes(self.vram[addr..addr + 2].try_into().unwrap());
                self.line_buf.bg[i][x as usize] = col & 0x7FFF;
            }
        }
    }

    fn render_mode4_bg(&mut self) {
        let i = 2;

        if !self.display_bg[i] {
            return;
        }

        let base_addr = self.frame_addr();
        let (cx, cy) = self.calc_left_for_line(i);

        for x in 0..SCREEN_WIDTH {
            if let Some((rx, ry)) = self.calc_refpoint_for_x(i, 240, 160, false, x, cx, cy) {
                let col_num = self.vram[(base_addr + (ry * 240 + rx)) as usize];
                if col_num != 0 {
                    self.line_buf.bg[i][x as usize] = self.bg_palette256(col_num as _);
                }
            }
        }
    }

    fn render_mode5_bg(&mut self) {
        let i = 2;

        if !self.display_bg[i] {
            return;
        }

        let base_addr = self.frame_addr();
        let (cx, cy) = self.calc_left_for_line(i);

        for x in 0..SCREEN_WIDTH {
            if let Some((rx, ry)) = self.calc_refpoint_for_x(i, 160, 128, false, x, cx, cy) {
                let addr = (base_addr + (ry * 160 + rx) * 2) as usize;
                let col = u16::from_le_bytes(self.vram[addr..addr + 2].try_into().unwrap());
                self.line_buf.bg[i][x as usize] = col & 0x7FFF;
            }
        }
    }

    fn calc_left_for_line(&mut self, i: usize) -> (i32, i32) {
        let dmx = self.bg[i].dmx as i16 as i32;
        let dmy = self.bg[i].dmy as i16 as i32;

        let cx = sign_extend(self.bg[i].x, 27);
        let cy = sign_extend(self.bg[i].y, 27);

        self.bg[i].x = (cx + dmx) as u32 & 0x0FFFFFFF;
        self.bg[i].y = (cy + dmy) as u32 & 0x0FFFFFFF;

        if self.bg[i].mosaic {
            let mh = self.bg_mosaic_v as u32 + 1;
            let mody = (self.y % mh) as i32;
            (cx - dmx * mody, cy - dmy * mody)
        } else {
            (cx, cy)
        }
    }

    fn calc_refpoint_for_x(
        &self,
        i: usize,
        w: u32,
        h: u32,
        wrapping: bool,
        x: u32,
        cx: i32,
        cy: i32,
    ) -> Option<(u32, u32)> {
        let relx = if self.bg[i].mosaic {
            let mw = self.bg_mosaic_h as u32 + 1;
            x / mw * mw
        } else {
            x
        };

        let dx = self.bg[i].dx as i16 as i32;
        let dy = self.bg[i].dy as i16 as i32;

        let rx = (cx + dx * relx as i32) >> 8;
        let ry = (cy + dy * relx as i32) >> 8;

        if wrapping {
            Some((rx as u32 % w, ry as u32 % h))
        } else if rx >= 0 && rx < w as i32 && ry >= 0 && ry < h as i32 {
            Some((rx as u32, ry as u32))
        } else {
            None
        }
    }

    fn frame_addr(&self) -> u32 {
        if !self.display_frame_select {
            0
        } else {
            0xA000
        }
    }

    fn render_obj(&mut self) {
        if !self.display_obj {
            return;
        }

        let num_of_hdots = if !self.hblank_obj_process {
            DOTS_PER_LINE
        } else {
            SCREEN_WIDTH
        };

        let mut avail_cycle = num_of_hdots * 4 - 6;

        for i in 0..128 {
            let oam = &self.oam[i * 8..i * 8 + 6];
            let rot = oam[1] & 1 != 0;
            let double = oam[1] & 2 != 0;

            // This case is not displayed
            if (double, rot) == (true, false) {
                continue;
            }

            let y = oam[0] as u32;

            // 00: normal
            // 01: semi-transparent
            // 10: obj window
            // 11: prohibited
            let mode = (oam[1] >> 2) & 3;
            if mode == 3 {
                continue;
            }

            let shape = (oam[1] >> 6) & 3;

            // prohibited
            if shape == 3 {
                continue;
            }

            let x = oam[2] as u32 | (oam[3] as u32 & 1) << 8;

            let size = (oam[3] >> 6) & 3;

            const OBJ_SIZE_TBL: [[(u32, u32); 4]; 3] = [
                [(8, 8), (16, 16), (32, 32), (64, 64)],
                [(16, 8), (32, 8), (32, 16), (64, 32)],
                [(8, 16), (8, 32), (16, 32), (32, 64)],
            ];

            let (ow, oh) = OBJ_SIZE_TBL[shape as usize][size as usize];
            let w = ow * if double { 2 } else { 1 };
            let h = oh * if double { 2 } else { 1 };

            let char_name = oam[4] as u32 | (oam[5] as u32 & 3) << 8;

            // On BG 3-5, Obj char ram is halved, so 0-511 are disabled
            if self.bg_mode >= 3 && char_name < 512 {
                continue;
            }

            let priority = (oam[5] >> 2) & 3;

            let color_256 = oam[1] & 0x20 != 0;

            let mosaic = oam[1] & 0x10 != 0;

            let scry = if mosaic {
                let mosaic_h = self.obj_mosaic_v as u32 + 1;
                self.y / mosaic_h * mosaic_h
            } else {
                self.y
            };

            let rely = if y + h > 256 {
                if !(scry < y + h - 256 && self.y < y + h - 256) {
                    continue;
                }
                256 + scry - y
            } else {
                if !(y <= scry && scry < y + h && y <= self.y && self.y < y + h) {
                    continue;
                }
                scry - y
            };

            let mosaic_w = if mosaic { self.obj_mosaic_h + 1 } else { 1 } as u32;

            if !rot {
                let hflip = oam[3] & 0x10 != 0;
                let vflip = oam[3] & 0x20 != 0;
                let palette_num = oam[5] >> 4;

                self.render_normal_obj(
                    hflip,
                    vflip,
                    color_256,
                    palette_num,
                    mode,
                    mosaic_w,
                    priority,
                    char_name,
                    w,
                    h,
                    x,
                    rely,
                );
            } else {
                let rot_param_num = (oam[3] >> 1) & 0x1F;
                let palette_num = oam[5] >> 4;

                self.render_rotate_obj(
                    rot_param_num,
                    color_256,
                    palette_num,
                    mode,
                    mosaic_w,
                    priority,
                    char_name,
                    ow,
                    oh,
                    w,
                    h,
                    x,
                    rely,
                );
            }

            // TODO: how many cycles for invisible objs?
            avail_cycle -= min(avail_cycle, num_of_render_cycle(w, rot));

            if avail_cycle == 0 {
                break;
            }
        }
    }

    fn render_normal_obj(
        &mut self,
        hflip: bool,
        vflip: bool,
        color256: bool,
        palette_num: u8,
        mode: u8,
        mosaic_w: u32,
        priority: u8,
        char_name: u32,
        w: u32,
        h: u32,
        x: u32,
        rely: u32,
    ) {
        let dim2 = !self.obj_format;
        let dy = if !vflip { rely } else { h - 1 - rely };

        for relx in 0..w {
            let sx = (x + relx) % 512;
            if sx >= 240 {
                continue;
            }
            let scrx = sx / mosaic_w * mosaic_w;
            let relx = if scrx < x { scrx + 512 - x } else { scrx - x };
            if relx >= w {
                continue;
            }

            let dx = if !hflip { relx } else { w - 1 - relx };

            let col_num = if !color256 {
                let c = self.get_obj_pixel16(char_name, dx, dy, w, dim2);
                if c != 0 {
                    palette_num * 16 + c
                } else {
                    0
                }
            } else {
                self.get_obj_pixel256(char_name, dx, dy, w, dim2)
            };
            self.put_obj_pixel(sx as _, col_num, mode, priority);
        }
    }

    fn render_rotate_obj(
        &mut self,
        rot_param_num: u8,
        color_256: bool,
        palette_num: u8,
        mode: u8,
        mosaic_w: u32,
        priority: u8,
        char_name: u32,
        ow: u32,
        oh: u32,
        w: u32,
        h: u32,
        x: u32,
        rely: u32,
    ) {
        let dim2 = !self.obj_format;

        let rot_param_base = rot_param_num as usize * 32;
        let rot_param = &self.oam[rot_param_base..rot_param_base + 32];
        let dx = i16::from_le_bytes(rot_param[6..8].try_into().unwrap()) as i32;
        let dmx = i16::from_le_bytes(rot_param[14..16].try_into().unwrap()) as i32;
        let dy = i16::from_le_bytes(rot_param[22..24].try_into().unwrap()) as i32;
        let dmy = i16::from_le_bytes(rot_param[30..32].try_into().unwrap()) as i32;

        let mut rx = (ow as i32 / 2) << 8;
        let mut ry = (oh as i32 / 2) << 8;

        let rdx = -(w as i32 / 2);
        rx += dx * rdx;
        ry += dy * rdx;

        let rdy = rely as i32 - (h as i32 / 2);
        rx += dmx * rdy;
        ry += dmy * rdy;

        for i in 0..w {
            let sx = (x + i) % 512;
            if sx >= 240 {
                continue;
            }
            let scrx = sx / mosaic_w * mosaic_w;
            let relx = if scrx < x { scrx + 512 - x } else { scrx - x } as i32;

            let rx2 = (rx + dx * relx) >> 8;
            let ry2 = (ry + dy * relx) >> 8;

            if !(rx2 >= 0 && rx2 < ow as i32 && ry2 >= 0 && ry2 < oh as i32) {
                continue;
            }

            let rx2 = rx2 as u32;
            let ry2 = ry2 as u32;

            let col_num = if !color_256 {
                let col_num = self.get_obj_pixel16(char_name, rx2, ry2, ow, dim2);
                if col_num != 0 {
                    palette_num * 16 + col_num
                } else {
                    0
                }
            } else {
                self.get_obj_pixel256(char_name, rx2, ry2, ow, dim2)
            };
            self.put_obj_pixel(sx as _, col_num, mode, priority);
        }
    }

    fn get_obj_pixel16(&self, char_name: u32, x: u32, y: u32, w: u32, dim2: bool) -> u8 {
        let tile_num = if dim2 {
            char_name + (y / 8) * 32 + x / 8
        } else {
            char_name + (y / 8) * (w / 8) + x / 8
        };
        let addr = tile_num * 32 + (y % 8) * 4 + x % 8 / 2;
        (self.vram[(OBJ_BASE_ADDR + addr) as usize] >> (x % 2 * 4)) & 0xf
    }

    fn get_obj_pixel256(&self, char_name: u32, x: u32, y: u32, w: u32, dim2: bool) -> u8 {
        let tile_num = if dim2 {
            // On 256 color and 2-dimensional mode, char name must be even number
            (char_name & !1) + (y / 8) * 32 + x / 8 * 2
        } else {
            char_name + ((y / 8) * (w / 8) + x / 8) * 2
        };
        let addr = tile_num * 32 + (y % 8) * 8 + x % 8;
        self.vram[(OBJ_BASE_ADDR + addr) as usize]
    }

    fn put_obj_pixel(&mut self, x: usize, col_num: u8, mode: u8, priority: u8) {
        if col_num == 0 {
            return;
        }

        let col = self.obj_palette256(col_num as _);
        match mode {
            // normal
            0 => {
                if self.line_buf.obj[x] & 0x8000 != 0 {
                    self.line_buf.obj[x] = col;
                    self.line_buf.obj_attr[x].set_priority(priority);
                    self.line_buf.obj_attr[x].set_semi_transparent(false);
                }
            }
            // semi-trans
            1 => {
                if self.line_buf.obj[x] & 0x8000 != 0 {
                    self.line_buf.obj[x] = col;
                    self.line_buf.obj_attr[x].set_priority(priority);
                    self.line_buf.obj_attr[x].set_semi_transparent(true);
                }
            }
            // obj-window
            2 => self.line_buf.obj_attr[x].set_window(true),
            _ => unreachable!(),
        }
    }

    fn eval_priority(&mut self) {
        if self.y == 0 {
            trace!("Eval priority:");

            for i in 0..2 {
                trace!("  - Window {i}:");
                trace!(
                    "    - region: ({}, {}) - ({}, {})",
                    self.window[i].l,
                    self.window[i].u,
                    self.window[i].r,
                    self.window[i].d,
                );
                trace!("    - display: {}", self.display_window[i],);
                trace!("    - ctrl: {:?}", self.winin[i]);
            }

            trace!(" - Objwin:");
            trace!("    - display: {}", self.display_obj_window);
            trace!("    - ctrl: {:?}", self.objwin);

            trace!(" - Winout:");
            trace!("    - ctrl: {:?}", self.winout);

            trace!("  - Display BG:  {:?}", self.display_bg,);
            trace!("  - Display Obj: {}", self.display_obj);
        }

        let y_in_win0 = self.display_window[0]
            && self.window[0].u as u32 <= self.y
            && self.y <= self.window[0].d as u32;
        let y_in_win1 = self.display_window[1]
            && self.window[1].u as u32 <= self.y
            && self.y <= self.window[1].d as u32;

        let winout_enable =
            self.display_window[0] || self.display_window[1] || self.display_obj_window;

        let any = WindowCtrl {
            display_bg: [true, true, true, true],
            display_obj: true,
            color_special_effect: false,
        };

        // FIXME: how to use color special effect flag on window?

        for x in 0..SCREEN_WIDTH {
            let in_win0 = y_in_win0 && self.window[0].l as u32 <= x && x <= self.window[0].r as u32;
            let in_win1 = y_in_win1 && self.window[1].l as u32 <= x && x <= self.window[1].r as u32;

            let win_ctrl = if in_win0 {
                &self.winin[0]
            } else if in_win1 {
                &self.winin[1]
            } else if self.line_buf.obj_attr[x as usize].window() {
                &self.objwin
            } else if winout_enable {
                &self.winout
            } else {
                &any
            }
            .clone();

            let x = x as usize;

            if self.display_obj && win_ctrl.display_obj {
                let col = self.line_buf.obj[x];
                if col & 0x8000 == 0 {
                    // eprintln!(
                    //     "x: {x} = {col:04X}, prio: {}",
                    //     self.line_buf.obj_attr[x].priority()
                    // );
                    self.put_surface_pixel(x, col, self.line_buf.obj_attr[x].priority(), 4);
                }
            }

            for i in 0..4 {
                if !(self.display_bg[i] && win_ctrl.display_bg[i]) {
                    continue;
                }

                let col = self.line_buf.bg[i][x];
                if col & 0x8000 == 0 {
                    self.put_surface_pixel(x, col, self.bg[i].priority, i as u8);
                }
            }
        }
    }

    fn put_surface_pixel(&mut self, x: usize, col: u16, priority: u8, attr: u8) {
        if self.line_buf.surface_priority[0][x] > priority {
            self.line_buf.surface[1][x] = self.line_buf.surface[0][x];
            self.line_buf.surface_priority[1][x] = self.line_buf.surface_priority[0][x];
            self.line_buf.surface_attr[1][x] = self.line_buf.surface_attr[0][x];

            self.line_buf.surface[0][x] = col;
            self.line_buf.surface_priority[0][x] = priority;
            self.line_buf.surface_attr[0][x] = attr
        } else if self.line_buf.surface_priority[1][x] > priority {
            self.line_buf.surface[1][x] = col;
            self.line_buf.surface_priority[1][x] = priority;
            self.line_buf.surface_attr[1][x] = attr
        }
    }

    fn color_special_effect(&mut self) {
        let back_drop = self.bg_palette256(0);

        // eprintln!("Color special effect: backdrop: 0x{:04X}", back_drop);

        for x in 0..SCREEN_WIDTH {
            let x = x as usize;
            let (ty, col) = if self.line_buf.surface_priority[0][x] == 4 {
                (0, back_drop)
            } else if self.line_buf.surface[1][x] == 4 {
                (1, self.line_buf.surface[0][x])
            } else {
                let c0 = self.line_buf.surface[0][x];
                let c1 = self.line_buf.surface[1][x];
                let a0 = self.line_buf.surface_attr[0][x];
                let a1 = self.line_buf.surface_attr[1][x];

                // TODO: overwrite effect by obj attr
                // TODO: disable effect by window

                match self.blend_ctrl.effect {
                    // None
                    0 => (2, c0),
                    // Alpha belnding
                    1 => {
                        if self.blend_ctrl.target[0] & (1 << a0) != 0
                            && self.blend_ctrl.target[1] & (1 << a1) != 0
                        {
                            (
                                3,
                                alpha_blend(c0, self.blend_ctrl.eva, c1, self.blend_ctrl.evb),
                            )
                        } else {
                            (4, c0)
                        }
                    }
                    // Brightness increase
                    2 => {
                        if self.blend_ctrl.target[0] & (1 << a0) != 0 {
                            (5, brightness_increase(c0, self.blend_ctrl.evy))
                        } else {
                            (6, c0)
                        }
                    }
                    // Brightness decrease
                    3 => {
                        if self.blend_ctrl.target[0] & (1 << a0) != 0 {
                            (7, brightness_decrease(c0, self.blend_ctrl.evy))
                        } else {
                            (8, c0)
                        }
                    }
                    _ => unreachable!(),
                }
            };

            self.line_buf.finished[x] = col;
        }
    }

    fn bg_palette256(&self, i: usize) -> u16 {
        u16::from_le_bytes(self.palette[i * 2..i * 2 + 2].try_into().unwrap()) & 0x7FFF
    }

    fn bg_palette16(&self, i: usize, j: usize) -> u16 {
        self.bg_palette256(i * 16 + j)
    }

    fn obj_palette256(&self, i: usize) -> u16 {
        self.bg_palette256(256 + i)
    }
}

fn alpha_blend(a: u16, eva: u8, b: u16, evb: u8) -> u16 {
    let ar = a & 0x1F;
    let ag = (a >> 5) & 0x1F;
    let ab = (a >> 10) & 0x1F;
    let br = b & 0x1F;
    let bg = (b >> 5) & 0x1F;
    let bb = (b >> 10) & 0x1F;
    let cr = alpha_blend_mono(ar, eva, br, evb);
    let cg = alpha_blend_mono(ag, eva, bg, evb);
    let cb = alpha_blend_mono(ab, eva, bb, evb);
    (cb << 10) | (cg << 5) | cr
}

fn alpha_blend_mono(a: u16, eva: u8, b: u16, evb: u8) -> u16 {
    min(31, (a * eva as u16 + b * evb as u16) / 16)
}

fn brightness_increase(c: u16, evy: u8) -> u16 {
    let r = c & 0x1F;
    let g = (c >> 5) & 0x1F;
    let b = (c >> 10) & 0x1F;
    let r = brightness_increase_mono(r, evy);
    let g = brightness_increase_mono(g, evy);
    let b = brightness_increase_mono(b, evy);
    (b << 10) | (g << 5) | r
}

fn brightness_increase_mono(y: u16, evy: u8) -> u16 {
    y + (31 - y) * evy as u16 / 16
}

fn brightness_decrease(c: u16, evy: u8) -> u16 {
    let r = c & 0x1F;
    let g = (c >> 5) & 0x1F;
    let b = (c >> 10) & 0x1F;
    let r = brightness_decrease_mono(r, evy);
    let g = brightness_decrease_mono(g, evy);
    let b = brightness_decrease_mono(b, evy);
    (b << 10) | (g << 5) | r
}

fn brightness_decrease_mono(y: u16, evy: u8) -> u16 {
    y - y * evy as u16 / 16
}

fn num_of_render_cycle(width: u32, rot: bool) -> u32 {
    if !rot {
        width
    } else {
        width * 2 + 10
    }
}

fn sign_extend(x: u32, sign: u32) -> i32 {
    let shift = 31 - sign;
    (x << shift) as i32 >> shift
}
