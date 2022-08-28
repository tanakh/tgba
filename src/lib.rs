mod backup;
mod bios;
mod bus;
mod consts;
mod context;
mod cpu;
mod dma;
mod gamepak;
mod interrupt;
mod ioreg_info;
mod lcd;
mod rom;
mod serial;
mod sound;
mod timer;
mod util;

use context::Context;
pub use rom::Rom;

use meru_interface::{
    AudioBuffer, CoreInfo, EmulatorCore, File, FrameBuffer, InputData, KeyConfig,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

pub struct Agb {
    ctx: Context,
}

const CORE_INFO: CoreInfo = CoreInfo {
    system_name: "Game Boy Advance (TGBA)",
    abbrev: "gba",
    file_extensions: &["gba"],
};

fn default_key_config() -> KeyConfig {
    use meru_interface::key_assign::*;

    #[rustfmt::skip]
    let keys = vec![
        ("Up", any!(keycode!(Up), pad_button!(0, DPadUp))),
        ("Down", any!(keycode!(Down), pad_button!(0, DPadDown))),
        ("Left", any!(keycode!(Left), pad_button!(0, DPadLeft))),
        ("Right", any!(keycode!(Right), pad_button!(0, DPadRight))),
        ("A", any!(keycode!(X), pad_button!(0, East))),
        ("B", any!(keycode!(Z), pad_button!(0, South))),
        ("L", any!(keycode!(A), pad_button!(0, LeftTrigger))),
        ("R", any!(keycode!(S), pad_button!(0, RightTrigger))),
        ("Start", any!(keycode!(Return), pad_button!(0, Start))),
        ("Select", any!(keycode!(RShift), pad_button!(0, Select))),
    ];

    KeyConfig {
        controllers: vec![keys.into_iter().map(|(k, v)| (k.to_string(), v)).collect()],
    }
}

#[derive(Default, Clone, JsonSchema, Serialize, Deserialize)]
pub struct Config {
    /// BIOS file
    bios: Option<File>,
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("BIOS must be specified")]
    BiosNotSpecified,
    #[error("{0}")]
    RomError(#[from] rom::RomError),
    #[error("deserialize failed: {0}")]
    DeserializeFailed(#[from] bincode::Error),
    #[error("{0}")]
    Io(#[from] std::io::Error),
}

impl EmulatorCore for Agb {
    type Config = Config;
    type Error = Error;

    fn core_info() -> &'static CoreInfo {
        &CORE_INFO
    }

    fn try_from_file(
        data: &[u8],
        backup: Option<&[u8]>,
        config: &Self::Config,
    ) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let bios = {
            let bios = config.bios.as_ref().ok_or(Error::BiosNotSpecified)?;
            bios.data()?
        };

        let rom = Rom::from_bytes(data)?;

        let mut ctx = Context::new(bios, rom, backup.map(|r| r.to_vec()));
        ctx.cpu.set_pc(&mut ctx.inner, 0);
        Ok(Agb { ctx })
    }

    fn game_info(&self) -> Vec<(String, String)> {
        use context::GamePak;
        let rom = self.ctx.gamepak().rom();

        let rom_size = rom.data.len();
        let rom_size = if rom_size < 1024 * 1024 {
            format!("{} KiB", rom_size as f64 / 1024_f64)
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

    fn set_config(&mut self, _config: &Self::Config) {}

    fn exec_frame(&mut self, render_graphics: bool) {
        use context::{Bus, Lcd, Sound};

        self.ctx.lcd_mut().set_render_graphics(render_graphics);
        self.ctx
            .lcd_mut()
            .frame_buffer_mut()
            .resize(consts::SCREEN_WIDTH as _, consts::SCREEN_HEIGHT as _);
        self.ctx.sound_mut().clear_buffer();

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

    fn reset(&mut self) {
        use context::{Bus, GamePak};

        let bios = self.ctx.bus().bios.clone();
        let rom = self.ctx.gamepak().rom().clone();
        let backup = self.ctx.backup().data();

        self.ctx = Context::new(bios, rom, backup);
    }

    fn frame_buffer(&self) -> &FrameBuffer {
        use context::Lcd;
        self.ctx.lcd().frame_buffer()
    }

    fn audio_buffer(&self) -> &AudioBuffer {
        use context::Sound;
        self.ctx.sound().audio_buffer()
    }

    fn default_key_config() -> KeyConfig {
        default_key_config()
    }

    fn set_input(&mut self, input: &InputData) {
        let mut agb_input = bus::KeyInput::default();

        for (key, value) in &input.controllers[0] {
            match key.as_str() {
                "A" => agb_input.a = *value,
                "B" => agb_input.b = *value,
                "Start" => agb_input.start = *value,
                "Select" => agb_input.select = *value,
                "L" => agb_input.l = *value,
                "R" => agb_input.r = *value,
                "Up" => agb_input.up = *value,
                "Down" => agb_input.down = *value,
                "Left" => agb_input.left = *value,
                "Right" => agb_input.right = *value,
                _ => unreachable!(),
            }
        }

        use context::Bus;
        self.ctx.set_key_input(&agb_input);
    }

    fn backup(&self) -> Option<Vec<u8>> {
        use context::GamePak;
        self.ctx.gamepak().backup().data()
    }

    fn save_state(&self) -> Vec<u8> {
        bincode::serialize(&self.ctx).unwrap()
    }

    fn load_state(&mut self, data: &[u8]) -> Result<(), Self::Error> {
        use context::{Bus, GamePak};

        let mut ctx: Context = bincode::deserialize(data)?;

        // Restore unsaved components
        std::mem::swap(
            self.ctx.gamepak_mut().rom_mut(),
            ctx.gamepak_mut().rom_mut(),
        );
        std::mem::swap(&mut self.ctx.bus_mut().bios, &mut ctx.bus_mut().bios);

        self.ctx = ctx;
        Ok(())
    }
}
