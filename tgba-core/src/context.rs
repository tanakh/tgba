use crate::{bus, cpu, io, rom::Rom};

pub trait Bus {
    fn read8(&mut self, addr: u32) -> u8;
    fn read16(&mut self, addr: u32) -> u16;
    fn read32(&mut self, addr: u32) -> u32;

    fn write8(&mut self, addr: u32, data: u8);
    fn write16(&mut self, addr: u32, data: u16);
    fn write32(&mut self, addr: u32, data: u32);
}

pub trait Io {
    fn io_read8(&mut self, addr: u32) -> u8;
    fn io_read16(&mut self, addr: u32) -> u16;
    fn io_read32(&mut self, addr: u32) -> u32;

    fn io_write8(&mut self, addr: u32, data: u8);
    fn io_write16(&mut self, addr: u32, data: u16);
    fn io_write32(&mut self, addr: u32, data: u32);
}

pub trait Interrupt {
    fn irq(&self) -> bool;
    fn set_irq(&mut self, irq: bool);
    fn fiq(&self) -> bool;
    fn set_fiq(&mut self, fiq: bool);
}

pub struct Context {
    pub cpu: cpu::Cpu<Inner>,
    pub inner: Inner,
}

pub struct Inner {
    pub bus: bus::Bus,
    pub inner: Inner2,
}

pub struct Inner2 {
    pub io: io::Io,
    pub inner: Inner3,
}

pub struct Inner3 {
    irq: bool,
    fiq: bool,
}

impl Context {
    pub fn new(bios: Vec<u8>, rom: Rom) -> Self {
        let cpu = cpu::Cpu::new();
        let bus = bus::Bus::new(bios, rom);
        let io = io::Io::new();

        Context {
            cpu,
            inner: Inner {
                bus,
                inner: Inner2 {
                    io,
                    inner: Inner3 {
                        irq: false,
                        fiq: false,
                    },
                },
            },
        }
    }
}

impl Bus for Inner {
    fn read8(&mut self, addr: u32) -> u8 {
        self.bus.read8(&mut self.inner, addr)
    }

    fn read16(&mut self, addr: u32) -> u16 {
        self.bus.read16(&mut self.inner, addr)
    }

    fn read32(&mut self, addr: u32) -> u32 {
        self.bus.read32(&mut self.inner, addr)
    }

    fn write8(&mut self, addr: u32, data: u8) {
        self.bus.write8(&mut self.inner, addr, data)
    }

    fn write16(&mut self, addr: u32, data: u16) {
        self.bus.write16(&mut self.inner, addr, data)
    }

    fn write32(&mut self, addr: u32, data: u32) {
        self.bus.write32(&mut self.inner, addr, data)
    }
}

impl Io for Inner2 {
    fn io_read8(&mut self, addr: u32) -> u8 {
        self.io.read8(&mut self.inner, addr)
    }

    fn io_read16(&mut self, addr: u32) -> u16 {
        self.io.read16(&mut self.inner, addr)
    }

    fn io_read32(&mut self, addr: u32) -> u32 {
        self.io.read32(&mut self.inner, addr)
    }

    fn io_write8(&mut self, addr: u32, data: u8) {
        self.io.write8(&mut self.inner, addr, data)
    }

    fn io_write16(&mut self, addr: u32, data: u16) {
        self.io.write16(&mut self.inner, addr, data)
    }

    fn io_write32(&mut self, addr: u32, data: u32) {
        self.io.write32(&mut self.inner, addr, data)
    }
}

impl Interrupt for Inner {
    fn irq(&self) -> bool {
        self.inner.irq()
    }

    fn set_irq(&mut self, irq: bool) {
        self.inner.set_irq(irq)
    }

    fn fiq(&self) -> bool {
        self.inner.fiq()
    }

    fn set_fiq(&mut self, fiq: bool) {
        self.inner.set_fiq(fiq)
    }
}

impl Interrupt for Inner2 {
    fn irq(&self) -> bool {
        self.inner.irq()
    }

    fn set_irq(&mut self, irq: bool) {
        self.inner.set_irq(irq)
    }

    fn fiq(&self) -> bool {
        self.inner.fiq()
    }

    fn set_fiq(&mut self, fiq: bool) {
        self.inner.set_fiq(fiq)
    }
}

impl Interrupt for Inner3 {
    fn irq(&self) -> bool {
        self.irq
    }

    fn set_irq(&mut self, irq: bool) {
        self.irq = irq
    }

    fn fiq(&self) -> bool {
        self.fiq
    }

    fn set_fiq(&mut self, fiq: bool) {
        self.fiq = fiq
    }
}
