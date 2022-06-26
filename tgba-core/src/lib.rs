mod backup;
mod bios;
mod bus;
mod consts;
mod context;
mod cpu;
mod dma;
mod gamepak;
mod interface;
mod interrupt;
mod ioreg_info;
mod lcd;
mod rom;
mod serial;
mod sound;
mod timer;
mod util;

use context::Context;

use interface::AudioBuf;
pub use interface::{FrameBuf, KeyInput};
pub use rom::Rom;

pub struct Agb {
    ctx: Context,
}

impl Agb {
    pub fn new(bios: Vec<u8>, rom: Rom, backup: Option<Vec<u8>>) -> Self {
        let mut ctx = Context::new(bios, rom, backup);
        ctx.cpu.set_pc(&mut ctx.inner, 0);
        Agb { ctx }
    }

    pub fn info(&self) -> Vec<(String, String)> {
        use context::GamePak;
        let rom = self.ctx.gamepak().rom();

        let rom_size = rom.data.len();
        let rom_size = if rom_size < 1 * 1024 * 1024 {
            format!("{} KiB", rom_size as f64 / 1024 as f64)
        } else {
            format!("{} MiB", rom_size as f64 / (1024 * 1024) as f64)
        };

        vec![
            (
                "Title".to_string(),
                String::from_utf8_lossy(&rom.title).to_string(),
            ),
            (
                "Game Code".to_string(),
                String::from_utf8_lossy(&rom.game_code).to_string(),
            ),
            (
                "Maker Code".to_string(),
                String::from_utf8_lossy(&rom.maker_code).to_string(),
            ),
            ("Main Unit Code".to_string(), rom.main_unit_code.to_string()),
            ("Device Type".to_string(), rom.device_type.to_string()),
            ("ROM Version".to_string(), rom.rom_version.to_string()),
            ("Backup Type".to_string(), rom.backup_type().to_string()),
            ("ROM Size".to_string(), rom_size),
        ]
    }

    pub fn reset(&mut self) {
        use context::{Bus, GamePak};

        let bios = self.ctx.bus().bios.clone();
        let rom = self.ctx.gamepak().rom().clone();
        let backup = self.ctx.backup().data();

        self.ctx = Context::new(bios, rom, backup);
    }

    pub fn exec_frame(&mut self, render_graphics: bool) {
        use context::{Bus, Lcd, Sound};

        self.ctx.sound_mut().clear_buf();
        self.ctx.lcd_mut().set_render_graphics(render_graphics);

        let start_frame = self.ctx.lcd().frame();
        while start_frame == self.ctx.lcd().frame() {
            if !self.ctx.dma_tick() {
                self.ctx.cpu.exec_one(&mut self.ctx.inner);
            }
            self.ctx.lcd_tick();
            self.ctx.sound_tick();
            self.ctx.bus_tick();
        }
    }

    pub fn frame_buf(&self) -> &FrameBuf {
        use context::Lcd;
        self.ctx.lcd().frame_buf()
    }

    pub fn audio_buf(&self) -> &AudioBuf {
        use context::Sound;
        self.ctx.sound().audio_buf()
    }

    pub fn set_key_input(&mut self, key_input: &KeyInput) {
        use context::Bus;
        self.ctx.set_key_input(key_input);
    }

    pub fn backup(&self) -> Option<Vec<u8>> {
        use context::GamePak;
        self.ctx.gamepak().backup().data()
    }

    pub fn save_state(&self) -> Vec<u8> {
        bincode::serialize(&self.ctx).unwrap()
    }

    pub fn load_state(&mut self, data: &[u8]) -> anyhow::Result<()> {
        use context::{Bus, GamePak, Lcd};
        use std::mem::swap;

        let mut ctx: Context = bincode::deserialize(data)?;

        // Restore unsaved components
        swap(
            self.ctx.gamepak_mut().rom_mut(),
            ctx.gamepak_mut().rom_mut(),
        );
        swap(&mut self.ctx.bus_mut().bios, &mut ctx.bus_mut().bios);
        swap(
            &mut self.ctx.lcd_mut().frame_buf,
            &mut ctx.lcd_mut().frame_buf,
        );

        self.ctx = ctx;
        Ok(())
    }
}
