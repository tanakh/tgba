use crate::{bus, cpu, interrupt, lcd, rom, sound};
use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Bus {
    fn read8(&mut self, addr: u32, first: bool) -> u8;
    fn read16(&mut self, addr: u32, first: bool) -> u16;
    fn read32(&mut self, addr: u32, first: bool) -> u32;

    fn write8(&mut self, addr: u32, data: u8, first: bool);
    fn write16(&mut self, addr: u32, data: u16, first: bool);
    fn write32(&mut self, addr: u32, data: u32, first: bool);
}

#[delegatable_trait]
pub trait Lcd {
    fn lcd(&self) -> &lcd::Lcd;
    fn lcd_mut(&mut self) -> &mut lcd::Lcd;

    fn lcd_tick(&mut self);

    fn lcd_read16(&mut self, addr: u32) -> u16;
    fn lcd_write16(&mut self, addr: u32, data: u16);
}

#[delegatable_trait]
pub trait Sound {
    fn sound_read8(&mut self, addr: u32) -> u8;
    fn sound_read16(&mut self, addr: u32) -> u16;

    fn sound_write8(&mut self, addr: u32, data: u8);
    fn sound_write16(&mut self, addr: u32, data: u16);
}

#[delegatable_trait]
pub trait Interrupt {
    fn interrupt(&self) -> &interrupt::Interrupt;
    fn interrupt_mut(&mut self) -> &mut interrupt::Interrupt;
}

#[delegatable_trait]
pub trait Timing {
    fn now(&self) -> u64;
    fn elapse(&mut self, elapse: u64);
}

#[derive(Delegate)]
#[delegate(Bus, target = "inner")]
#[delegate(Lcd, target = "inner")]
#[delegate(Sound, target = "inner")]
#[delegate(Interrupt, target = "inner")]
#[delegate(Timing, target = "inner")]
pub struct Context {
    pub cpu: cpu::Cpu<Inner>,
    pub inner: Inner,
}

impl Context {
    pub fn new(bios: Vec<u8>, rom: rom::Rom) -> Self {
        let cpu = cpu::Cpu::new();
        let bus = bus::Bus::new(bios, rom);
        let lcd = lcd::Lcd::new();
        let sound = sound::Sound::new();
        let interrupt = interrupt::Interrupt::new();

        Context {
            cpu,
            inner: Inner {
                bus,
                inner: Inner2 {
                    lcd,
                    sound,
                    inner: Inner3 { interrupt, now: 0 },
                },
            },
        }
    }
}

#[derive(Delegate)]
#[delegate(Lcd, target = "inner")]
#[delegate(Sound, target = "inner")]
#[delegate(Interrupt, target = "inner")]
#[delegate(Timing, target = "inner")]
pub struct Inner {
    pub bus: bus::Bus,
    pub inner: Inner2,
}

impl Bus for Inner {
    fn read8(&mut self, addr: u32, first: bool) -> u8 {
        self.bus.read8(&mut self.inner, addr, first)
    }
    fn read16(&mut self, addr: u32, first: bool) -> u16 {
        self.bus.read16(&mut self.inner, addr, first)
    }
    fn read32(&mut self, addr: u32, first: bool) -> u32 {
        self.bus.read32(&mut self.inner, addr, first)
    }

    fn write8(&mut self, addr: u32, data: u8, first: bool) {
        self.bus.write8(&mut self.inner, addr, data, first)
    }
    fn write16(&mut self, addr: u32, data: u16, first: bool) {
        self.bus.write16(&mut self.inner, addr, data, first)
    }
    fn write32(&mut self, addr: u32, data: u32, first: bool) {
        self.bus.write32(&mut self.inner, addr, data, first)
    }
}

#[derive(Delegate)]
#[delegate(Interrupt, target = "inner")]
#[delegate(Timing, target = "inner")]
pub struct Inner2 {
    lcd: lcd::Lcd,
    sound: sound::Sound,
    inner: Inner3,
}

impl Lcd for Inner2 {
    fn lcd(&self) -> &lcd::Lcd {
        &self.lcd
    }

    fn lcd_mut(&mut self) -> &mut lcd::Lcd {
        &mut self.lcd
    }

    fn lcd_tick(&mut self) {
        self.lcd.tick(&mut self.inner);
    }

    fn lcd_read16(&mut self, addr: u32) -> u16 {
        self.lcd.read16(&mut self.inner, addr)
    }

    fn lcd_write16(&mut self, addr: u32, data: u16) {
        self.lcd.write16(&mut self.inner, addr, data)
    }
}

impl Sound for Inner2 {
    fn sound_read8(&mut self, addr: u32) -> u8 {
        self.sound.read8(&mut self.inner, addr)
    }

    fn sound_read16(&mut self, addr: u32) -> u16 {
        self.sound.read16(&mut self.inner, addr)
    }

    fn sound_write8(&mut self, addr: u32, data: u8) {
        self.sound.write8(&mut self.inner, addr, data)
    }

    fn sound_write16(&mut self, addr: u32, data: u16) {
        self.sound.write16(&mut self.inner, addr, data)
    }
}

pub struct Inner3 {
    interrupt: interrupt::Interrupt,
    now: u64,
}

impl Interrupt for Inner3 {
    fn interrupt(&self) -> &interrupt::Interrupt {
        &self.interrupt
    }
    fn interrupt_mut(&mut self) -> &mut interrupt::Interrupt {
        &mut self.interrupt
    }
}

impl Timing for Inner3 {
    fn now(&self) -> u64 {
        self.now
    }
    fn elapse(&mut self, elapse: u64) {
        self.now += elapse;
    }
}
