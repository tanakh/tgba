use std::cmp::min;

use bitvec::prelude::*;
use log::{info, trace};

use crate::{
    consts::{CLOCK_PER_DOT, SCREEN_HEIGHT, SCREEN_WIDTH, VISIBLE_HEIGHT, VISIBLE_WIDTH},
    context::{Interrupt, Timing},
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

    line_buf: Vec<u16>,
    win_line_buf: Vec<Vec<u8>>,
    obj_line_buf: Vec<u8>,
    obj_line_buf_attr: Vec<ObjAttr>,
    frame_buf: FrameBuf,
}

#[derive(Default)]
pub struct FrameBuf {
    width: u32,
    height: u32,
    buf: Vec<Pixel>,
}

impl FrameBuf {
    pub fn new(width: u32, height: u32) -> Self {
        FrameBuf {
            width,
            height,
            buf: vec![Pixel::new(0, 0, 0); (width * height) as usize],
        }
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn pixel(&self, x: u32, y: u32) -> &Pixel {
        &self.buf[(y * self.width + x) as usize]
    }

    pub fn pixel_mut(&mut self, x: u32, y: u32) -> &mut Pixel {
        &mut self.buf[(y * self.width + x) as usize]
    }
}

#[derive(Clone)]
pub struct Pixel {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Pixel {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }

    pub fn from_u16(p: u16) -> Self {
        Self {
            r: extend_color(p & 0x1F),
            g: extend_color((p >> 5) & 0x1F),
            b: extend_color((p >> 10) & 0x1F),
        }
    }
}

fn extend_color(col5: u16) -> u8 {
    ((col5 << 3) | (col5 >> 2)) as u8
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
            line_buf: vec![0; VISIBLE_WIDTH as usize],
            win_line_buf: vec![vec![0; VISIBLE_WIDTH as usize]; 4],
            obj_line_buf: vec![0; VISIBLE_WIDTH as usize],
            obj_line_buf_attr: vec![ObjAttr::default(); VISIBLE_WIDTH as usize],
            frame_buf: FrameBuf::new(VISIBLE_WIDTH, VISIBLE_HEIGHT),
            ..Default::default()
        }
    }

    pub fn frame(&self) -> u64 {
        self.frame
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

        if self.y < VISIBLE_HEIGHT && self.x == VISIBLE_WIDTH {
            // TODO: HBLANK
            info!("Enter HBLANK: frame:{}, y:{:03}", self.frame, self.y);

            self.render_line();

            if self.hblank_irq_enable {
                ctx.interrupt_mut().set_interrupt(InterruptKind::HBlank);
            }
        }

        if self.x >= SCREEN_WIDTH {
            self.x -= SCREEN_WIDTH;
            self.y += 1;

            trace!("Frame:{}, Line:{:03}", self.frame, self.y);

            if self.y == VISIBLE_HEIGHT {
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

            if self.y >= SCREEN_HEIGHT {
                self.y -= SCREEN_HEIGHT;
                self.frame += 1;
            }
        }
    }

    fn vblank(&self) -> bool {
        self.y >= VISIBLE_HEIGHT
    }

    fn hblank(&self) -> bool {
        self.y < VISIBLE_HEIGHT && self.x >= VISIBLE_WIDTH
    }

    fn vcount_match(&self) -> bool {
        self.y == self.vcount as u32
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        match addr {
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
                self.vcount = v[8..=15].load();
            }

            // VCOUNT
            0x006 => {}

            // BGxCNT
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

impl Lcd {
    fn render_line(&mut self) {
        if self.force_blank {
            for x in 0..VISIBLE_WIDTH {
                *self.frame_buf.pixel_mut(x, self.y) = Pixel::new(255, 255, 255);
            }
            return;
        }

        let back_drop = self.bg_palette256(0);
        self.line_buf.fill(back_drop);

        self.render_obj();

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

        // self.eval_priority();
        // self.process_color_effect();

        for x in 0..VISIBLE_WIDTH {
            *self.frame_buf.pixel_mut(x, self.y) = Pixel::from_u16(self.line_buf[x as usize]);
        }
    }

    fn render_text_bg(&mut self, i: usize) {
        if !self.display_bg[i] {
            return;
        }

        todo!();
    }

    fn render_rotate_bg(&mut self, i: usize) {
        if !self.display_bg[i] {
            return;
        }

        // todo!();
    }

    fn render_mode3_bg(&mut self) {
        if !self.display_bg[2] {
            return;
        }

        todo!();
    }

    fn render_mode4_bg(&mut self) {
        if !self.display_bg[2] {
            return;
        }

        let base_addr = if !self.display_frame_select {
            0
        } else {
            0xA000
        } + self.y * 0xF0;

        trace!("Mode4");
        trace!("Frame select: {}", self.display_frame_select);

        // let line_buf = &mut self.win_line_buf[2];

        for x in 0..VISIBLE_WIDTH {
            // line_buf[x as usize] = self.vram[base_addr as usize + x as usize];
            self.line_buf[x as usize] =
                self.bg_palette256(self.vram[base_addr as usize + x as usize] as usize);
        }
    }

    fn render_mode5_bg(&mut self) {
        if !self.display_bg[2] {
            return;
        }

        todo!();
    }

    fn render_obj(&mut self) {
        if !self.display_obj {
            return;
        }

        self.obj_line_buf_attr.fill(ObjAttr::default());

        let num_of_hdots = if !self.hblank_obj_process {
            SCREEN_WIDTH
        } else {
            VISIBLE_WIDTH
        };

        let mut avail_cycle = num_of_hdots * 4 - 6;

        let obj_base_addr = 0x10000;

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

            let mosaic = oam[1] & 0x10 != 0;
            let color_256 = oam[1] & 0x20 != 0;

            let shape = (oam[1] >> 6) & 3;

            // prohibited
            if shape == 3 {
                continue;
            }

            let x = oam[2] as u32 | (oam[3] as u32 & 1) << 8;

            let size = (oam[3] >> 6) & 3;

            const SIZE_TBL: [[(u32, u32); 4]; 3] = [
                [(8, 8), (16, 16), (32, 32), (64, 64)],
                [(16, 8), (32, 8), (32, 16), (64, 32)],
                [(8, 16), (8, 32), (16, 32), (32, 64)],
            ];

            let (w, h) = SIZE_TBL[shape as usize][size as usize];

            let char_name = oam[4] as u32 | (oam[5] as u32 & 3) << 8;

            // On BG 3-5, Obj char ram is halved, so 0-511 are disabled
            if self.bg_mode >= 3 && char_name < 512 {
                continue;
            }

            let priority = (oam[5] >> 2) & 3;

            let y_range = if y + h > 256 {
                (0, (y + h) % 256)
            } else {
                (y, y + h)
            };

            if !(y_range.0 <= self.y && y_range.1 <= self.y) {
                continue;
            }

            if !rot {
                // Normal OBj

                let hflip = oam[3] & 0x10 != 0;
                let vflip = oam[3] & 0x20 != 0;

                let dy = self.y - y_range.0;
                let dy = if !vflip { dy } else { h - 1 - dy };

                let mut put_pixel = |x: usize, col: u8| {
                    match mode {
                        // normal
                        0 => {
                            if self.obj_line_buf[x] == 0 {
                                self.obj_line_buf[x] = col;
                                self.obj_line_buf_attr[x].set_priority(priority);
                                self.obj_line_buf_attr[x].set_semi_transparent(false);
                            }
                        }
                        // semi-trans
                        1 => {
                            if self.obj_line_buf[x] == 0 {
                                self.obj_line_buf[x] = col;
                                self.obj_line_buf_attr[x].set_priority(priority);
                                self.obj_line_buf_attr[x].set_semi_transparent(true);
                            }
                        }
                        // obj-window
                        2 => self.obj_line_buf_attr[x].set_window(true),
                        _ => unreachable!(),
                    }
                };

                if !color_256 {
                    let palette_num = oam[5] >> 4;

                    if !self.obj_format {
                        // 2-dim
                        let l_char = char_name + (dy / 8) * 32;

                        for dx in 0..w {
                            let cx = ((x + dx) % 512) as usize;
                            if cx >= 240 {
                                continue;
                            }

                            let bx = dx / 8;
                            let ox = dx % 8;
                            let addr = (l_char + bx * 2) * 32 + (dy % 8) * 4 + ox / 2;
                            let col =
                                (self.vram[(obj_base_addr + addr) as usize] >> (ox % 2 * 4)) & 0xf;
                            if col != 0 {
                                put_pixel(cx, palette_num * 16 + col);
                            }
                        }
                    } else {
                        // 1-dim
                        let l_char = char_name + (dy / 8) * (w / 8);

                        for dx in 0..w {
                            let cx = ((x + dx) % 512) as usize;
                            if cx >= 240 {
                                continue;
                            }

                            let bx = dx / 8;
                            let ox = dx % 8;
                            let addr = (l_char + bx) * 32 + (dy % 8) * 4 + ox / 2;
                            let col =
                                (self.vram[(obj_base_addr + addr) as usize] >> (ox % 2 * 4)) & 0xf;
                            if col != 0 {
                                put_pixel(cx, palette_num * 16 + col);
                            }
                        }
                    }
                } else {
                    if !self.obj_format {
                        // 2-dim
                        // On 256 color and 2-dimensional mode,
                        // char name must be even number
                        let char_name = char_name / 2;

                        let l_char = char_name + (dy / 8) * 32;

                        for dx in 0..w {
                            let cx = ((x + dx) % 512) as usize;
                            if cx >= 240 {
                                continue;
                            }

                            let bx = dx / 8;
                            let addr = (l_char + bx * 2) * 32 + (dy % 8) * 8 + (dx % 8);

                            assert!(
                                addr < 0x8000,
                                "obj[{i}]: shape: {shape:02b}, char_name: {char_name:03X}, x: {x}, y: {y}, w: {w}, h: {h}"
                            );

                            let col = self.vram[(obj_base_addr + addr) as usize];
                            if col != 0 {
                                put_pixel(cx, col);
                            }
                        }
                    } else {
                        // 1-dim
                        let l_char = char_name + (dy / 8) * (w / 8);

                        for dx in 0..w {
                            let cx = ((x + dx) % 512) as usize;
                            if cx >= 240 {
                                continue;
                            }

                            let bx = dx / 8;
                            let addr = (l_char + bx) * 64 + (dy % 8) * 8 + (dx % 8);
                            let col = self.vram[(obj_base_addr + addr) as usize];
                            if col != 0 {
                                put_pixel(cx, col);
                            }
                        }
                    }
                }
            } else {
                // todo!("Rotate/Scaling Obj");

                let rot_param = (oam[3] >> 1) & 0x1F;
            }

            // TODO: how many cycles for invisible objs?
            avail_cycle -= min(avail_cycle, num_of_render_cycle(w, rot));

            if avail_cycle == 0 {
                break;
            }
        }
    }

    fn eval_priority(&mut self) {
        let y_in_win0 = self.display_window[0]
            && self.window[0].u as u32 <= self.y
            && self.y < self.window[0].d as u32;
        let y_in_win1 = self.display_window[1]
            && self.window[1].u as u32 <= self.y
            && self.y < self.window[1].d as u32;

        for x in 0..VISIBLE_WIDTH {
            let in_win0 = y_in_win0 && self.window[0].l as u32 <= x && x <= self.window[0].r as u32;
            let in_win1 = y_in_win1 && self.window[1].l as u32 <= x && x <= self.window[1].r as u32;

            let win_ctrl = if in_win0 {
                &self.winin[0]
            } else if in_win1 {
                &self.winin[1]
            } else if self.obj_line_buf_attr[x as usize].window() {
                &self.objwin
            } else {
                &self.winout
            };

            let mut pixel = self.bg_palette256(0);

            'priority: for p in 0..4 {
                if self.display_obj
                    && win_ctrl.display_obj
                    && self.obj_line_buf_attr[x as usize].priority() == p
                {
                    let col = self.obj_line_buf[x as usize];
                    if col != 0 {
                        pixel = self.obj_palette256(col as _);
                        break 'priority;
                    }
                }

                for i in 0..4 {
                    if self.display_bg[i] && win_ctrl.display_bg[i] && self.bg[i].priority == p {
                        let col = self.win_line_buf[i][x as usize];
                        if col != 0 {
                            pixel = self.bg_palette256(col as _);
                            break 'priority;
                        }
                    }
                }
            }

            self.line_buf[x as usize] = pixel;
        }
    }

    fn process_color_effect(&mut self) {
        todo!();
    }

    fn bg_palette256(&self, i: usize) -> u16 {
        u16::from_le_bytes(self.palette[i * 2..i * 2 + 2].try_into().unwrap())
    }

    fn bg_palette16(&self, i: usize, j: usize) -> u16 {
        self.bg_palette256(i * 16 + j)
    }

    fn obj_palette256(&self, i: usize) -> u16 {
        self.bg_palette256(256 + i)
    }

    fn obj_palette16(&self, i: usize, j: usize) -> u16 {
        self.bg_palette256(256 + i * 16 + j)
    }
}

fn num_of_render_cycle(width: u32, rot: bool) -> u32 {
    if !rot {
        width
    } else {
        width * 2 + 10
    }
}
