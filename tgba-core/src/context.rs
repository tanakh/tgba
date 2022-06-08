use crate::{backup, bus, cpu, gamepak, interrupt, lcd, rom, sound, KeyInput};
use ambassador::{delegatable_trait, Delegate};
use serde::{Deserialize, Serialize};

#[delegatable_trait]
pub trait Bus {
    fn bus(&self) -> &bus::Bus;
    fn bus_mut(&mut self) -> &mut bus::Bus;

    fn read8(&mut self, addr: u32, first: bool) -> u8;
    fn read16(&mut self, addr: u32, first: bool) -> u16;
    fn read32(&mut self, addr: u32, first: bool) -> u32;

    fn write8(&mut self, addr: u32, data: u8, first: bool);
    fn write16(&mut self, addr: u32, data: u16, first: bool);
    fn write32(&mut self, addr: u32, data: u32, first: bool);

    fn bus_tick(&mut self);
    fn dma_tick(&mut self) -> bool;

    fn set_key_input(&mut self, key_input: &KeyInput);
}

#[delegatable_trait]
pub trait Lcd {
    fn lcd(&self) -> &lcd::Lcd;
    fn lcd_mut(&mut self) -> &mut lcd::Lcd;

    fn lcd_tick(&mut self);

    fn lcd_read(&mut self, addr: u32) -> u8;
    fn lcd_write(&mut self, addr: u32, data: u8);
}

#[delegatable_trait]
pub trait Sound {
    fn sound(&self) -> &sound::Sound;
    fn sound_mut(&mut self) -> &mut sound::Sound;

    fn sound_tick(&mut self);
    fn sound_timer_overflow(&mut self, ch: u8);

    fn sound_read(&mut self, addr: u32) -> u8;
    fn sound_write(&mut self, addr: u32, data: u8);
}

#[delegatable_trait]
pub trait Interrupt {
    fn interrupt(&self) -> &interrupt::Interrupt;
    fn interrupt_mut(&mut self) -> &mut interrupt::Interrupt;
}

#[delegatable_trait]
pub trait SoundDma {
    fn sound_dma_request(&self, ch: u8) -> bool;
    fn set_sound_dma_request(&mut self, ch: u8, data: bool);
}

#[delegatable_trait]
pub trait GamePak {
    fn gamepak(&self) -> &gamepak::GamePak;
    fn gamepak_mut(&mut self) -> &mut gamepak::GamePak;

    fn backup(&self) -> &backup::Backup;
    fn backup_mut(&mut self) -> &mut backup::Backup;
}

#[delegatable_trait]
pub trait Timing {
    fn now(&self) -> u64;
    fn elapse(&mut self, elapse: u64);
}

#[derive(Delegate, Serialize, Deserialize)]
#[delegate(Bus, target = "inner")]
#[delegate(Lcd, target = "inner")]
#[delegate(Sound, target = "inner")]
#[delegate(GamePak, target = "inner")]
#[delegate(Interrupt, target = "inner")]
#[delegate(Timing, target = "inner")]
pub struct Context {
    pub cpu: cpu::Cpu<Inner>,
    pub inner: Inner,
}

impl Context {
    pub fn new(bios: Vec<u8>, rom: rom::Rom, backup: Option<Vec<u8>>) -> Self {
        let cpu = cpu::Cpu::new();
        let bus = bus::Bus::new(bios);
        let lcd = lcd::Lcd::new();
        let sound = sound::Sound::new();
        let gamepak = gamepak::GamePak::new(rom, backup);
        let interrupt = interrupt::Interrupt::new();

        Context {
            cpu,
            inner: Inner {
                bus,
                inner: Inner2 {
                    lcd,
                    sound,
                    gamepak,
                    inner: Inner3 {
                        interrupt,
                        sound_dma_request: [false, false],
                        now: 0,
                    },
                },
            },
        }
    }
}

#[derive(Delegate, Serialize, Deserialize)]
#[delegate(Lcd, target = "inner")]
#[delegate(Sound, target = "inner")]
#[delegate(GamePak, target = "inner")]
#[delegate(Interrupt, target = "inner")]
#[delegate(Timing, target = "inner")]
pub struct Inner {
    pub bus: bus::Bus,
    pub inner: Inner2,
}

impl Default for Inner {
    fn default() -> Self {
        // Dummy implementation for CPU serde
        unreachable!()
    }
}

impl Bus for Inner {
    fn bus(&self) -> &bus::Bus {
        &self.bus
    }
    fn bus_mut(&mut self) -> &mut bus::Bus {
        &mut self.bus
    }

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

    fn bus_tick(&mut self) {
        self.bus.tick(&mut self.inner);
    }

    fn dma_tick(&mut self) -> bool {
        self.bus.dma_tick(&mut self.inner)
    }

    fn set_key_input(&mut self, key_input: &KeyInput) {
        self.bus.set_key_input(&mut self.inner, key_input);
    }
}

#[derive(Delegate, Serialize, Deserialize)]
#[delegate(Interrupt, target = "inner")]
#[delegate(SoundDma, target = "inner")]
#[delegate(Timing, target = "inner")]
pub struct Inner2 {
    lcd: lcd::Lcd,
    sound: sound::Sound,
    gamepak: gamepak::GamePak,
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

    fn lcd_read(&mut self, addr: u32) -> u8 {
        self.lcd.read(&mut self.inner, addr)
    }
    fn lcd_write(&mut self, addr: u32, data: u8) {
        self.lcd.write(&mut self.inner, addr, data)
    }
}

impl Sound for Inner2 {
    fn sound(&self) -> &sound::Sound {
        &self.sound
    }
    fn sound_mut(&mut self) -> &mut sound::Sound {
        &mut self.sound
    }

    fn sound_tick(&mut self) {
        self.sound.tick(&mut self.inner);
    }
    fn sound_timer_overflow(&mut self, ch: u8) {
        self.sound.timer_overflow(&mut self.inner, ch);
    }

    fn sound_read(&mut self, addr: u32) -> u8 {
        self.sound.read(&mut self.inner, addr)
    }
    fn sound_write(&mut self, addr: u32, data: u8) {
        self.sound.write(&mut self.inner, addr, data)
    }
}

impl GamePak for Inner2 {
    fn gamepak(&self) -> &gamepak::GamePak {
        &self.gamepak
    }
    fn gamepak_mut(&mut self) -> &mut gamepak::GamePak {
        &mut self.gamepak
    }

    fn backup(&self) -> &backup::Backup {
        self.gamepak.backup()
    }
    fn backup_mut(&mut self) -> &mut backup::Backup {
        self.gamepak.backup_mut()
    }
}

#[derive(Serialize, Deserialize)]
pub struct Inner3 {
    interrupt: interrupt::Interrupt,
    sound_dma_request: [bool; 2],
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

impl SoundDma for Inner3 {
    fn sound_dma_request(&self, ch: u8) -> bool {
        self.sound_dma_request[ch as usize]
    }

    fn set_sound_dma_request(&mut self, ch: u8, data: bool) {
        self.sound_dma_request[ch as usize] = data;
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
