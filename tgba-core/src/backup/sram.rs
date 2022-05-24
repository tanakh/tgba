pub struct Sram {
    data: Vec<u8>,
}

impl Sram {
    pub fn new() -> Self {
        Self {
            data: vec![0; 0x10000],
        }
    }

    pub fn read(&self, addr: u32) -> u8 {
        self.data[addr as usize]
    }

    pub fn write(&mut self, addr: u32, data: u8) {
        self.data[addr as usize] = data;
    }
}
