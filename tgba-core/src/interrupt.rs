use log::info;

#[derive(Default)]
pub struct Interrupt {
    master_enable: bool,
    enable: u16,
    request: u16,
    halt: bool,
    stop: bool,
}

#[derive(Debug)]
pub enum InterruptKind {
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

impl Interrupt {
    pub fn new() -> Interrupt {
        Interrupt::default()
    }

    pub fn irq(&self) -> bool {
        self.master_enable && (self.enable & self.request) != 0
    }

    pub fn fiq(&self) -> bool {
        false
    }

    pub fn master_enable(&self) -> bool {
        self.master_enable
    }

    pub fn set_master_enable(&mut self, enable: bool) {
        self.master_enable = enable;
    }

    pub fn enable(&self) -> u16 {
        self.enable
    }

    pub fn set_enable(&mut self, enable: u16) {
        self.enable = enable;
    }

    pub fn request(&self) -> u16 {
        self.request
    }

    pub fn reset_request(&mut self, request: u16) {
        self.request &= !request;
    }

    pub fn set_interrupt(&mut self, source: InterruptKind) {
        info!("Set interrupt: {source:?}");
        self.request |= 1 << source as u16;
    }

    pub fn halt(&self) -> bool {
        self.halt
    }

    pub fn set_halt(&mut self, halt: bool) {
        self.halt = halt;
    }

    pub fn stop(&self) -> bool {
        self.stop
    }

    pub fn set_stop(&mut self, stop: bool) {
        self.stop = stop;
    }
}
