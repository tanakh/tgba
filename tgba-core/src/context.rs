use crate::{bus, cpu, rom::Rom};

pub trait Bus {
    fn read8(&mut self, addr: u32) -> u8;
    fn read16(&mut self, addr: u32) -> u16;
    fn read32(&mut self, addr: u32) -> u32;

    fn write8(&mut self, addr: u32, data: u8);
    fn write16(&mut self, addr: u32, data: u16);
    fn write32(&mut self, addr: u32, data: u32);
}

pub trait Interrupt {
    fn irq(&self) -> bool;
    fn set_irq(&mut self, irq: bool);
    fn fiq(&self) -> bool;
    fn set_fiq(&mut self, fiq: bool);
}

pub struct Context {
    pub cpu: cpu::Cpu<InnerContext>,
    pub inner: InnerContext,
}

pub struct InnerContext {
    pub bus: bus::Bus,
    pub inner: InnerContext2,
}

pub struct InnerContext2 {
    irq: bool,
    fiq: bool,
}

impl Context {
    pub fn new(bios: Vec<u8>, rom: Rom) -> Self {
        let cpu = cpu::Cpu::new();
        let bus = bus::Bus::new(bios, rom);

        Context {
            cpu,
            inner: InnerContext {
                bus,
                inner: InnerContext2 {
                    irq: false,
                    fiq: false,
                },
            },
        }
    }
}

impl Bus for InnerContext {
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

impl Interrupt for InnerContext {
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

impl Interrupt for InnerContext2 {
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
