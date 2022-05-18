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

#[derive(Clone, Default)]
pub struct KeyInput {
    pub a: bool,
    pub b: bool,
    pub select: bool,
    pub start: bool,
    pub right: bool,
    pub left: bool,
    pub up: bool,
    pub down: bool,
    pub r: bool,
    pub l: bool,
}
