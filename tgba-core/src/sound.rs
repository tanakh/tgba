use std::{cmp::min, collections::VecDeque};

use bitvec::prelude::*;

use crate::{
    consts::AUDIO_SAMPLES_PER_SECOND,
    context::{Interrupt, SoundDma, Timing},
    interface::{AudioBuf, AudioSample},
    util::{pack, trait_alias},
};

trait_alias!(pub trait Context = Timing + SoundDma + Interrupt);

pub struct Sound {
    power_on: bool,
    output_ratio_gb: u8, // 00: 1/4, 01: 1/2, 10: Full, 11: Prohibited
    output_ratio_direct_sound: [bool; 2], // 0: 1/2, 1: Full

    channel_ctrl: [ChannelCtrl; 2], // 0=right, 1=left

    pulse: [Pulse; 2],
    wave: Wave,
    noise: Noise,
    direct_sound: [DirectSound; 2],

    prev_clock: u64,
    freq_counter: u64,
    frame_counter: u64,
    sampling_counter: u32,

    // #[serde(skip)]
    audio_buffer: AudioBuf,
}

impl Sound {
    pub fn new() -> Sound {
        Sound {
            power_on: false,
            output_ratio_gb: 0,
            output_ratio_direct_sound: [false; 2],

            channel_ctrl: Default::default(),

            pulse: [Pulse::new(true), Pulse::new(false)],
            wave: Default::default(),
            noise: Default::default(),
            direct_sound: [DirectSound::new(0), DirectSound::new(1)],

            prev_clock: 0,
            freq_counter: 0,
            frame_counter: 0,
            sampling_counter: 0,
            audio_buffer: Default::default(),
        }
    }

    pub fn audio_buf(&self) -> &AudioBuf {
        &self.audio_buffer
    }

    pub fn clear_buf(&mut self) {
        self.audio_buffer.buf.clear();
    }

    fn set_power(&mut self, on: bool) {
        if self.power_on == on {
            return;
        }

        self.power_on = on;
        // self.frame_sequencer_div.reset();
        // self.frame_sequencer_step = 7;
        self.channel_ctrl = Default::default();

        let ch1_len = self.pulse[0].length;
        let ch2_len = self.pulse[1].length;
        let ch3_len = self.wave.length;
        let ch4_len = self.noise.length;

        self.pulse[0].reset();
        self.pulse[1].reset();
        self.wave.reset();
        self.noise.reset();

        self.pulse[0].length = ch1_len;
        self.pulse[1].length = ch2_len;
        self.wave.length = ch3_len;
        self.noise.length = ch4_len;
    }

    pub fn timer_overflow(&mut self, ctx: &mut impl Context, timer_ch: u8) {
        if !self.power_on {
            return;
        }

        for sound_ch in 0..2 {
            self.direct_sound[sound_ch].timer_overflow(ctx, timer_ch);
        }
    }

    pub fn tick(&mut self, ctx: &mut impl Context) {
        let now = ctx.now();
        let elapsed = now - self.prev_clock;
        self.prev_clock = now;

        // System clock = 16.78MHz
        // Tick each channel at 131.072KHz
        // Prescaler = 1/128

        self.freq_counter += elapsed;
        let ticks = self.freq_counter / 128;
        self.freq_counter %= 128;

        for _ in 0..ticks {
            self.tick_128k(ctx);
        }
    }

    fn tick_128k(&mut self, ctx: &mut impl Context) {
        if self.power_on {
            self.frame_counter += 1;

            // Count up at 512Hz (128K / 256 = 512)
            let frame_seq_step = self.frame_counter / 256;

            // Step   Length Ctr  Vol Env     Sweep
            // ---------------------------------------
            // 0      Clock       -           -
            // 1      -           -           -
            // 2      Clock       -           Clock
            // 3      -           -           -
            // 4      Clock       -           -
            // 5      -           -           -
            // 6      Clock       -           Clock
            // 7      -           Clock       -
            // ---------------------------------------
            // Rate   256 Hz      64 Hz       128 Hz

            let length_tick = frame_seq_step % 2 == 0;
            let envelope_tick = frame_seq_step % 8 == 7;
            let sweep_tick = frame_seq_step % 4 == 2;

            self.pulse[0].tick(length_tick, envelope_tick, sweep_tick);
            self.pulse[1].tick(length_tick, envelope_tick, false);
            self.wave.tick(length_tick);
            self.noise.tick(length_tick, envelope_tick);
        }

        // AUDIO_SAMPLE_PER_FRAME samples per DOTS_PER_LINE * LINES_PER_FRAME
        const TICKS_PER_SECOND: u32 = 128 * 1024;

        self.sampling_counter += AUDIO_SAMPLES_PER_SECOND;
        if self.sampling_counter >= TICKS_PER_SECOND {
            self.sampling_counter -= TICKS_PER_SECOND;
            let sample = self.mix_output();
            self.audio_buffer.buf.push(sample);
        }
    }

    fn mix_output(&mut self) -> AudioSample {
        if !self.power_on {
            return AudioSample::new(0, 0);
        }

        let dac = |output: Option<u8>| match output {
            None => 0,
            Some(output) => (output as i16 - 8) << 5,
        };

        let ch_output = [
            dac(self.pulse[0].output()),
            dac(self.pulse[1].output()),
            dac(self.wave.output()),
            dac(self.noise.output()),
        ];

        let mut output = [0, 0];

        for (i, out) in output.iter_mut().enumerate() {
            for (j, ch_out) in ch_output.iter().enumerate() {
                if self.channel_ctrl[i].output_ch[j] {
                    *out += *ch_out;
                }
            }
            *out *= self.channel_ctrl[i].volume as i16 + 1;
        }

        match self.output_ratio_gb {
            0 => {
                output[0] /= 4;
                output[1] /= 4;
            }
            1 => {
                output[0] /= 2;
                output[1] /= 2;
            }
            2 => {}
            _ => panic!("Invalid Output Ratio for GB sound"),
        }

        for i in 0..2 {
            let out = match self.direct_sound[i].current_output {
                None => 0,
                Some(output) => (output as i8 as i16) << 1,
            };

            // eprintln!("DS[{i}] = {out}");

            let ratio = if !self.output_ratio_direct_sound[i] {
                2
            } else {
                1
            };

            for j in 0..2 {
                if self.direct_sound[i].output[j] {
                    output[j] += out * 64 / ratio;
                }
            }
        }

        AudioSample::new(output[0], output[1])
    }
}

#[derive(Default, Debug)]
struct ChannelCtrl {
    volume: u8,
    output_ch: [bool; 4],
}

#[derive(Debug, Default)]
struct Pulse {
    has_sweep_unit: bool,
    sweep_period: u8,
    sweep_negate: bool,
    sweep_shift: u8,

    duty: u8,
    length: u8, // Sound Length = (64-t1)*(1/256) seconds
    initial_volume: u8,
    envelope_inc: bool,
    envelope_period: u8, // Length of 1 step = n*(1/64) sec
    frequency: u16,      // Frequency = 131072/(2048-x) Hz
    length_enable: bool,

    on: bool,
    current_volume: u8,
    envelope_timer: u8,

    sweep_enable: bool,
    freq_calculated_in_negate_mode: bool,
    current_frequency: u16,
    sweep_timer: u8,

    frequency_timer: u16,
    phase: usize,

    length_tick_in: bool,
    prev_length_tick: bool,
    envelope_tick_in: bool,
    prev_envelope_tick: bool,
    sweep_tick_in: bool,
    prev_sweep_tick: bool,
}

impl Pulse {
    fn new(has_sweep_unit: bool) -> Self {
        Self {
            has_sweep_unit,
            ..Default::default()
        }
    }

    fn reset(&mut self) {
        *self = Self::new(self.has_sweep_unit);
    }

    // Square 1
    // NR10 -PPP NSSS Sweep period, negate, shift
    // NR11 DDLL LLLL Duty, Length load (64-L)
    // NR12 VVVV APPP Starting volume, Envelope add mode, period
    // NR13 FFFF FFFF Frequency LSB
    // NR14 TL-- -FFF Trigger, Length enable, Frequency MSB

    // Square 2
    //      ---- ---- Not used
    // NR21 DDLL LLLL Duty, Length load (64-L)
    // NR22 VVVV APPP Starting volume, Envelope add mode, period
    // NR23 FFFF FFFF Frequency LSB
    // NR24 TL-- -FFF Trigger, Length enable, Frequency MSB

    fn read(&mut self, regno: usize) -> u8 {
        match regno {
            // NR10: Channel 1 Sweep register (R/W)
            0 => pack! {
                7     => true,
                4..=6 => self.sweep_period,
                3     => self.sweep_negate,
                0..=2 => self.sweep_shift,
            },
            // NR11/NR21: Channel 1/2 Sound length/Wave pattern duty (R/W)
            // Only bits 7-6 can be read.
            1 => pack!(6..=7 => self.duty, 0..=5 => !0),
            // NR12/NR22: Channel 1/2 Envelope (R/W)
            2 => pack! {
                4..=7 => self.initial_volume,
                3     => self.envelope_inc,
                0..=2 => self.envelope_period,
            },
            // NR13/NR23: Channel 1/2 Frequency lo (W)
            3 => !0,
            // NR14/NR24: Channel 1/2 Frequency hi (R/W)
            // Only bit 6 can be read
            4 => pack! {
                7 => true,
                6 => self.length_enable,
                0..=5 => !0,
            },
            _ => unreachable!(),
        }
    }

    fn write(&mut self, regno: usize, data: u8) {
        match regno {
            // NR10: Channel 1 Sweep register (R/W)
            0 => {
                let prev_sweep_negate = self.sweep_negate;

                let v = data.view_bits::<Lsb0>();
                self.sweep_period = v[4..=6].load();
                self.sweep_negate = v[3];
                self.sweep_shift = v[0..=2].load();

                // neg -> pos after freq calculation disables channel
                if self.sweep_enable
                    && self.freq_calculated_in_negate_mode
                    && (prev_sweep_negate && !self.sweep_negate)
                {
                    self.on = false;
                    self.freq_calculated_in_negate_mode = false;
                }
            }
            // NR11/NR21: Channel1/2 Sound length/Wave pattern duty (R/W)
            1 => {
                let v = data.view_bits::<Lsb0>();
                self.duty = v[6..=7].load();
                self.length = 64 - v[0..=5].load::<u8>();
            }
            // NR12/NR22: Channel 1/2 Volume Envelope (R/W)
            2 => {
                let v = data.view_bits::<Lsb0>();
                self.initial_volume = v[4..=7].load();
                self.envelope_inc = v[3];
                self.envelope_period = v[0..=2].load();
            }
            // NR13/NR23: Channel 1/2 Frequency lo (W)
            3 => self.frequency.view_bits_mut::<Lsb0>()[0..=7].store(data),
            // NR14/NR24: Channel 1/2 Frequency hi (R/W)
            4 => {
                let v = data.view_bits::<Lsb0>();
                self.frequency.view_bits_mut::<Lsb0>()[8..=10].store(v[0..=2].load::<u16>());

                let prev_length_enable = self.length_enable;
                self.length_enable = v[6];

                // Extra length clogking
                if self.length_tick_in && !prev_length_enable && self.length_enable {
                    self.length_tick();
                }

                if v[7] {
                    self.trigger();
                }
            }
            _ => unreachable!(),
        }
        if !self.dac_enable() {
            self.on = false;
        }
    }

    fn dac_enable(&self) -> bool {
        self.initial_volume > 0 || self.envelope_inc
    }

    fn trigger(&mut self) {
        self.on = self.dac_enable();
        self.freq_calculated_in_negate_mode = false;

        if self.length == 0 {
            self.length = 64;
            if self.length_tick_in && self.length_enable {
                self.length_tick();
            }
        }

        self.frequency_timer = 2048 - self.frequency;

        // The volume envelope and sweep timers treat a period of 0 as 8.
        self.envelope_timer = if self.envelope_period == 0 {
            8
        } else {
            self.envelope_period
        };
        self.current_volume = self.initial_volume;

        self.sweep_timer = self.sweep_period();
        self.sweep_enable = self.sweep_period != 0 || self.sweep_shift != 0;
        self.current_frequency = self.frequency;
        if self.sweep_shift != 0 && self.new_freq() > 2047 {
            self.on = false;
        }
    }

    fn new_freq(&mut self) -> u16 {
        if self.sweep_negate {
            self.freq_calculated_in_negate_mode = true;
            self.current_frequency - (self.current_frequency >> self.sweep_shift)
        } else {
            self.current_frequency + (self.current_frequency >> self.sweep_shift)
        }
    }

    fn sweep_period(&self) -> u8 {
        if self.sweep_period == 0 {
            8
        } else {
            self.sweep_period
        }
    }

    fn tick(&mut self, length_tick: bool, envelope_tick: bool, sweep_tick: bool) {
        self.frequency_timer = self.frequency_timer.saturating_sub(1);
        if self.frequency_timer == 0 {
            self.frequency_timer = 2048 - self.frequency;
            self.phase = (self.phase + 1) % 8;
        }

        self.length_tick_in = length_tick;
        self.envelope_tick_in = envelope_tick;
        self.sweep_tick_in = sweep_tick;

        self.update_tick();
    }

    fn update_tick(&mut self) {
        let length_tick = self.length_tick_in;
        if !self.prev_length_tick && length_tick && self.length_enable {
            self.length_tick();
        }
        self.prev_length_tick = length_tick;

        let envelope_tick = self.envelope_tick_in;
        if !self.prev_envelope_tick && envelope_tick {
            self.envelope_tick();
        }
        self.prev_envelope_tick = envelope_tick;

        let sweep_tick = self.sweep_tick_in;
        if !self.prev_sweep_tick && sweep_tick {
            self.sweep_tick();
        }
        self.prev_sweep_tick = sweep_tick;
    }

    fn length_tick(&mut self) {
        self.length = self.length.saturating_sub(1);
        if self.length == 0 {
            self.on = false;
        }
    }

    fn envelope_tick(&mut self) {
        if self.envelope_timer > 0 {
            self.envelope_timer -= 1;
            if self.envelope_timer == 0 && self.envelope_period > 0 {
                self.envelope_timer = self.envelope_period;

                if self.envelope_inc {
                    self.current_volume = min(15, self.current_volume + 1);
                } else {
                    self.current_volume = self.current_volume.saturating_sub(1);
                }
            }
        }
    }

    fn sweep_tick(&mut self) {
        let prev_timer = self.sweep_timer;
        self.sweep_timer = self.sweep_timer.saturating_sub(1);

        if prev_timer > 0 && self.sweep_timer == 0 {
            self.sweep_timer = self.sweep_period();
            if self.sweep_enable && self.sweep_period > 0 {
                let new_freq = self.new_freq();
                if new_freq <= 2047 && self.sweep_shift > 0 {
                    self.current_frequency = new_freq;
                    self.frequency = new_freq;
                }
                // recalculate new frequency
                if self.new_freq() > 2047 {
                    self.on = false;
                }
            }
        }
    }

    // return Some(0..=15) value if the channel is enabled, otherwise None
    fn output(&self) -> Option<u8> {
        const WAVEFORM: [[u8; 8]; 4] = [
            [0, 0, 0, 0, 0, 0, 0, 1],
            [1, 0, 0, 0, 0, 0, 0, 1],
            [1, 0, 0, 0, 0, 1, 1, 1],
            [0, 1, 1, 1, 1, 1, 1, 0],
        ];

        if !self.on {
            None
        } else {
            Some(WAVEFORM[self.duty as usize][self.phase as usize] * self.current_volume)
        }
    }
}

#[derive(Default)]
struct Wave {
    enable: bool,
    length: u16,      // Sound Length = (256-t1)*(1/256) seconds
    output_level: u8, // 0 => mute, 1 => 100%, 2 => 50%, 3 => 25%
    frequency: u16,
    length_enable: bool,
    ram: [u8; 0x20],
    ram_bank: bool, // 0: Bank 0, 1: Bank 1
    steps: bool,    // 0: 32 steps, 1: 64 steps

    on: bool,
    frequency_timer: u16,
    sample_latch: u8,
    pos: u8,

    length_tick_in: bool,
    prev_length_tick: bool,
}

impl Wave {
    fn reset(&mut self) {
        // Powering APU shouldn't affect wave
        let t = self.ram;
        *self = Default::default();
        self.ram = t;
    }

    fn read_ram(&mut self, addr: u32) -> u8 {
        self.ram[(self.ram_bank as usize) * 0x10 + addr as usize]
    }

    fn write_ram(&mut self, addr: u32, data: u8) {
        self.ram[(self.ram_bank as usize) * 0x10 + addr as usize] = data;
    }

    fn read(&mut self, regno: usize) -> u8 {
        match regno {
            // NR30: Channel 3 Sound on/off (R/W)
            0 => pack!(7 => self.enable, 0..=6 => !0),
            // NR31: Channel 3 Sound length (R/W)
            1 => !0, // ???
            // NR32: Channel 3 Select output level (R/W)
            2 => pack!(5..=6 => self.output_level, 7 => true, 0..=4 => !0),
            // NR33: Channel 3 Frequency lo (W)
            3 => !0,
            // NR34: Channel 3 Frequency hi (R/W)
            // Only bit 6 can be read
            4 => pack!(6 => self.length_enable, 7 => true, 0..=5 => !0),
            _ => unreachable!(),
        }
    }

    fn write(&mut self, regno: usize, data: u8) {
        match regno {
            // NR30: Channel 3 Sound on/off (R/W)
            0 => {
                let v = data.view_bits::<Lsb0>();
                self.steps = v[5];
                self.ram_bank = v[6];
                self.enable = v[7];
            }
            // NR31: Channel 3 Sound length (R/W)
            1 => self.length = 256 - data as u16,
            // NR32: Channel 3 Select output level (R/W)
            2 => self.output_level = data.view_bits::<Lsb0>()[5..=6].load(),
            // NR33: Channel 3 Frequency lo (W)
            3 => self.frequency.view_bits_mut::<Lsb0>()[0..=7].store(data),
            // NR34: Channel 3 Frequency hi (R/W)
            4 => {
                let v = data.view_bits::<Lsb0>();
                self.frequency.view_bits_mut::<Lsb0>()[8..=10].store(v[0..=2].load::<u16>());
                self.length_enable = v[6];
                self.update_tick();
                if v[7] {
                    self.trigger();
                }
            }
            _ => unreachable!(),
        }
        if !self.dac_enable() {
            self.on = false;
        }
        self.update_tick();
    }

    fn dac_enable(&self) -> bool {
        self.enable
    }

    fn trigger(&mut self) {
        self.on = self.dac_enable();
        if self.length == 0 {
            self.length = 256;
        }
        self.frequency_timer = (2048 - self.frequency) * 2;
        self.pos = 0;
    }

    fn tick(&mut self, length_tick: bool) {
        self.frequency_timer = self.frequency_timer.saturating_sub(1);
        if self.frequency_timer == 0 {
            self.frequency_timer = (2048 - self.frequency) * 2;
            self.pos = (self.pos + 1) % 32;
            let v = self.ram[self.pos as usize / 2];
            self.sample_latch = if self.pos % 2 == 0 { v >> 4 } else { v & 0x0F };
        }

        self.length_tick_in = length_tick;
        self.update_tick();
    }

    fn update_tick(&mut self) {
        loop {
            let length_tick = self.length_tick_in && self.length_enable && self.length > 0;
            if self.prev_length_tick == length_tick {
                break;
            }
            if !self.prev_length_tick && length_tick {
                self.length_tick();
            }
            self.prev_length_tick = length_tick;
        }
    }

    fn length_tick(&mut self) {
        if self.length_enable {
            self.length = self.length.saturating_sub(1);
            if self.length == 0 {
                self.on = false;
            }
        }
    }

    fn output(&self) -> Option<u8> {
        if self.on {
            Some(if self.output_level == 0 {
                0
            } else {
                self.sample_latch >> (self.output_level - 1)
            })
        } else {
            None
        }
    }
}

#[derive(Default)]
struct Noise {
    length: u8, // Sound Length = (64-t1)*(1/256) seconds
    initial_volume: u8,
    envelope_inc: bool,
    envelope_period: u8, // Length of 1 step = n*(1/64) sec
    clock_shift: u8,
    lsfr_width: bool, // false=15bits, true=7bits
    divisor_code: u8, // Frequency = 524288 Hz / divisor / 2^(clock_shift+1)
    length_enable: bool,

    on: bool,
    current_volume: u8,
    envelope_timer: u8,
    divisor_timer: u8,
    shift_clock_timer: u16,
    lsfr: u16,
    sample_acc: usize,
    sample_count: usize,

    length_tick_in: bool,
    prev_length_tick: bool,
    envelope_tick_in: bool,
    prev_envelope_tick: bool,
}

static DIVISOR: [u8; 8] = [8, 16, 32, 48, 64, 80, 96, 112];

impl Noise {
    fn reset(&mut self) {
        *self = Self::default();
    }

    // Noise
    // NR41 --LL LLLL Length load (64-L)
    // NR42 VVVV APPP Starting volume, Envelope add mode, period
    // NR43 SSSS WDDD Clock shift, Width mode of LFSR, Divisor code
    // NR44 TL-- ---- Trigger, Length enable

    fn read(&mut self, regno: usize) -> u8 {
        match regno {
            // NR41: Channel 4 Sound length (R/W)
            1 => !0,
            // NR42: Channel 4 Volume Envelope (R/W)
            2 => pack! {
                4..=7 => self.initial_volume,
                3     => self.envelope_inc,
                0..=2 => self.envelope_period,
            },
            // NR43: Channel 4 Polynomial Counter (R/W)
            3 => pack! {
                4..=7 => self.clock_shift,
                3     => self.lsfr_width,
                0..=2 => self.divisor_code,
            },
            // NR44: Channel 4 Counter/consecutive; initial (R/W)
            // Only bit 6 can be read
            4 => pack!(6 => self.length_enable, 7 => true, 0..=5 => !0),
            _ => unreachable!(),
        }
    }

    fn write(&mut self, regno: usize, data: u8) {
        match regno {
            // NR41: Channel 4 Sound length (R/W)
            1 => self.length = 64 - data.view_bits::<Lsb0>()[0..=5].load::<u8>(),
            // NR42: Channel 4 Volume Envelope (R/W)
            2 => {
                let v = data.view_bits::<Lsb0>();
                self.initial_volume = v[4..=7].load();
                self.envelope_inc = v[3];
                self.envelope_period = v[0..=2].load();
            }
            // NR43: Channel 4 Polynomial Counter (R/W)
            3 => {
                let v = data.view_bits::<Lsb0>();
                self.clock_shift = v[4..=7].load();
                self.lsfr_width = v[3];
                self.divisor_code = v[0..=2].load();
            }
            // NR44: Channel 4 Counter/consecutive; initial (R/W)
            4 => {
                let v = data.view_bits::<Lsb0>();
                self.length_enable = v[6];
                self.update_tick();
                if v[7] {
                    self.trigger();
                }
            }
            _ => unreachable!(),
        }
        if !self.dac_enable() {
            self.on = false;
        }
        self.update_tick();
    }

    fn dac_enable(&self) -> bool {
        self.initial_volume > 0 || self.envelope_inc
    }

    fn trigger(&mut self) {
        self.on = self.dac_enable();
        if self.length == 0 {
            self.length = 64;
        }
        self.current_volume = self.initial_volume;
        self.lsfr = 0x7fff;
        self.divisor_timer = DIVISOR[self.divisor_code as usize] / 2;
        self.shift_clock_timer = 1 << (self.clock_shift + 1);

        log::debug!(
            "NOISE ch trigger: {}Hz, divisor={}, shfit={}",
            524288.0
                / (if self.divisor_code == 0 {
                    0.5
                } else {
                    self.divisor_code as f64
                })
                / 2.0_f64.powi(self.clock_shift as i32 + 1),
            self.divisor_code,
            self.clock_shift,
        );
    }

    fn tick(&mut self, length_tick: bool, envelope_tick: bool) {
        self.shift_clock_timer = self.shift_clock_timer.saturating_sub(1);
        if self.shift_clock_timer == 0 {
            self.shift_clock_timer = 1 << (self.clock_shift + 1);
            self.divisor_timer = self.divisor_timer.saturating_sub(1);
            if self.divisor_timer == 0 {
                self.sample_acc += ((self.lsfr & 1) ^ 1) as usize;
                self.sample_count += 1;

                self.divisor_timer = DIVISOR[self.divisor_code as usize] / 2;
                let b = (self.lsfr & 1) ^ ((self.lsfr >> 1) & 1);
                self.lsfr = if !self.lsfr_width {
                    (self.lsfr >> 1) | (b << 14)
                } else {
                    ((self.lsfr >> 1) & !(1 << 6)) | (b << 6)
                };
            }
        }

        self.length_tick_in = length_tick;
        self.envelope_tick_in = envelope_tick;
        self.update_tick();
    }

    fn update_tick(&mut self) {
        loop {
            let length_tick = self.length_tick_in && self.length_enable && self.length > 0;
            if self.prev_length_tick == length_tick {
                break;
            }

            if !self.prev_length_tick && length_tick {
                self.length_tick();
            }
            self.prev_length_tick = length_tick;
        }

        if !self.prev_envelope_tick && self.envelope_tick_in {
            self.envelope_tick();
        }
        self.prev_envelope_tick = self.envelope_tick_in;
    }

    fn length_tick(&mut self) {
        if self.length_enable {
            self.length = self.length.saturating_sub(1);
            if self.length == 0 {
                self.on = false;
            }
        }
    }

    fn envelope_tick(&mut self) {
        self.envelope_timer = self.envelope_timer.saturating_sub(1);
        if self.envelope_timer == 0 && self.envelope_period > 0 {
            self.envelope_timer = self.envelope_period;

            if self.envelope_inc {
                self.current_volume = min(15, self.current_volume + 1);
            } else {
                self.current_volume = self.current_volume.saturating_sub(1);
            }
        }
    }

    fn output(&mut self) -> Option<u8> {
        if !self.on {
            None
        } else {
            let sample_acc = self.sample_acc + ((self.lsfr & 1) ^ 1) as usize;
            let sample_count = self.sample_count + 1;
            let ret = sample_acc * self.current_volume as usize / sample_count;
            self.sample_acc = 0;
            self.sample_count = 0;
            Some(ret as u8)
        }
    }
}

struct DirectSound {
    ch: u8,
    output: [bool; 2],
    timer_ch: u8,
    current_output: Option<u8>,
    fifo: VecDeque<u8>,
}

impl DirectSound {
    fn new(ch: u8) -> Self {
        DirectSound {
            ch,
            output: [false, false],
            timer_ch: 0,
            current_output: None,
            fifo: VecDeque::new(),
        }
    }

    fn reset(&mut self) {
        self.fifo.clear();
        self.current_output = None;
    }

    fn push_fifo(&mut self, data: u8) {
        while self.fifo.len() + 1 > 32 {
            self.fifo.pop_front();
        }
        self.fifo.push_back(data);
    }

    fn timer_overflow(&mut self, ctx: &mut impl Context, ch: u8) {
        if ch != self.timer_ch {
            return;
        }

        self.current_output = self.fifo.pop_front();

        if self.fifo.len() <= 16 {
            // eprintln!(
            //     "Sound DMA Requested {}: current: {:?}",
            //     self.ch, self.current_output
            // );

            ctx.set_sound_dma_request(self.ch, true);
        }
    }
}

impl Sound {
    pub fn read8(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        let data = match addr {
            0x060 => self.pulse[0].read(0),
            0x061 => 0,
            0x062 => self.pulse[0].read(1),
            0x063 => self.pulse[0].read(2),
            0x064 => self.pulse[0].read(3),
            0x065 => self.pulse[0].read(4),
            0x066..=0x067 => 0,

            0x068 => self.pulse[1].read(1),
            0x069 => self.pulse[1].read(2),
            0x06A..=0x06B => 0,
            0x06C => self.pulse[1].read(3),
            0x06D => self.pulse[1].read(4),
            0x06E..=0x06F => 0,

            0x070 => self.wave.read(0),
            0x071 => 0,
            0x072 => self.wave.read(1),
            0x073 => self.wave.read(2),
            0x074 => self.wave.read(3),
            0x075 => self.wave.read(4),
            0x076..=0x077 => 0,

            0x078 => self.noise.read(1),
            0x079 => self.noise.read(2),
            0x07A..=0x07B => 0,
            0x07C => self.noise.read(3),
            0x07D => self.noise.read(4),
            0x07E..=0x07F => 0,

            // // NR50: Channel control / ON-OFF / Volume (R/W)
            // 0xFF24 => pack! {
            //     7     => self.channel_ctrl[1].vin_enable,
            //     4..=6 => self.channel_ctrl[1].volume,
            //     3     => self.channel_ctrl[0].vin_enable,
            //     0..=2 => self.channel_ctrl[0].volume,
            // },
            // // NR51: Selection of Sound output terminal (R/W)
            // 0xFF25 => pack! {
            //     7 => self.channel_ctrl[1].output_ch[3],
            //     6 => self.channel_ctrl[1].output_ch[2],
            //     5 => self.channel_ctrl[1].output_ch[1],
            //     4 => self.channel_ctrl[1].output_ch[0],
            //     3 => self.channel_ctrl[0].output_ch[3],
            //     2 => self.channel_ctrl[0].output_ch[2],
            //     1 => self.channel_ctrl[0].output_ch[1],
            //     0 => self.channel_ctrl[0].output_ch[0],
            // },
            // // NR52: Sound on/off (R/W)
            // 0xFF26 => pack! {
            //     7 => self.power_on,
            //     4..=6 => !0,
            //     3 => self.noise.on,
            //     2 => self.wave.on,
            //     1 => self.pulse[1].on,
            //     0 => self.pulse[0].on,
            // },

            // 0xFF27..=0xFF2F => !0,

            // // Wave Pattern RAM
            // 0xFF30..=0xFF3F => self.wave.ram[(addr & 0xf) as usize],

            // // PCM12
            // 0xFF76 => pack! {
            //     4..=7 => self.pulse[1].output().unwrap_or(0),
            //     0..=3 => self.pulse[0].output().unwrap_or(0),
            // },
            // // PCM34
            // 0xFF77 => pack! {
            //     4..=7 => self.noise.output().unwrap_or(0),
            //     0..=3 => self.wave.output().unwrap_or(0),
            // },

            // NR51: Selection of Sound output terminal (R/W)
            0x081 => pack! {
                7 => self.channel_ctrl[1].output_ch[3],
                6 => self.channel_ctrl[1].output_ch[2],
                5 => self.channel_ctrl[1].output_ch[1],
                4 => self.channel_ctrl[1].output_ch[0],
                3 => self.channel_ctrl[0].output_ch[3],
                2 => self.channel_ctrl[0].output_ch[2],
                1 => self.channel_ctrl[0].output_ch[1],
                0 => self.channel_ctrl[0].output_ch[0],
            },

            // SOUNDBIAS
            0x088 => 0x00,
            0x089 => 0x02,

            _ => todo!("Sound read: 0x{addr:03X}"),
        };

        data
    }

    pub fn read16(&mut self, ctx: &mut impl Context, addr: u32) -> u16 {
        let lo = self.read8(ctx, addr);
        let hi = self.read8(ctx, addr + 1);
        (hi as u16) << 8 | lo as u16
    }

    pub fn write8(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        match addr {
            0x060 => self.pulse[0].write(0, data),
            0x061 => {}
            0x062 => self.pulse[0].write(1, data),
            0x063 => self.pulse[0].write(2, data),
            0x064 => self.pulse[0].write(3, data),
            0x065 => self.pulse[0].write(4, data),
            0x066..=0x067 => {}

            0x068 => self.pulse[1].write(1, data),
            0x069 => self.pulse[1].write(2, data),
            0x06A..=0x06B => {}
            0x06C => self.pulse[1].write(3, data),
            0x06D => self.pulse[1].write(4, data),
            0x06E..=0x06F => {}

            0x070 => self.wave.write(0, data),
            0x071 => {}
            0x072 => self.wave.write(1, data),
            0x073 => self.wave.write(2, data),
            0x074 => self.wave.write(3, data),
            0x075 => self.wave.write(4, data),
            0x076..=0x077 => {}

            0x078 => self.noise.write(1, data),
            0x079 => self.noise.write(2, data),
            0x07A..=0x07B => {}
            0x07C => self.noise.write(3, data),
            0x07D => self.noise.write(4, data),
            0x07E..=0x07F => {}

            // NR50: Channel control / ON-OFF / Volume (R/W)
            0x080 => {
                let v = data.view_bits::<Lsb0>();
                self.channel_ctrl[1].volume = v[4..=6].load();
                self.channel_ctrl[0].volume = v[0..=2].load();
            }

            // NR51: Selection of Sound output terminal (R/W)
            0x081 => {
                let v = data.view_bits::<Lsb0>();
                self.channel_ctrl[1].output_ch[3] = v[7];
                self.channel_ctrl[1].output_ch[2] = v[6];
                self.channel_ctrl[1].output_ch[1] = v[5];
                self.channel_ctrl[1].output_ch[0] = v[4];
                self.channel_ctrl[0].output_ch[3] = v[3];
                self.channel_ctrl[0].output_ch[2] = v[2];
                self.channel_ctrl[0].output_ch[1] = v[1];
                self.channel_ctrl[0].output_ch[0] = v[0];
            }

            // SOUNDCNT_H
            0x082 => {
                let v = data.view_bits::<Lsb0>();
                self.output_ratio_gb = v[0..=1].load();
                self.output_ratio_direct_sound[0] = v[2];
                self.output_ratio_direct_sound[1] = v[3];
            }
            0x083 => {
                let v = data.view_bits::<Lsb0>();
                for ch in 0..2 {
                    self.direct_sound[0].output[0] = v[ch * 4];
                    self.direct_sound[0].output[1] = v[ch * 4 + 1];
                    self.direct_sound[0].timer_ch = v[ch * 4 + 2] as u8;
                    if v[ch * 4 + 3] {
                        self.direct_sound[0].reset();
                    }
                }
            }

            // NR52: Sound on/off (R/W)
            0x084 => {
                self.set_power(data.view_bits::<Lsb0>()[7]);
            }
            0x085 => {}

            // SOUNDBIAS
            0x088 | 0x089 => {}

            // Waveform RAM
            0x090..=0x09F => self.wave.write_ram(addr & 0xf, data),

            // Sound FIFO
            0x0A0..=0x0A3 => self.direct_sound[0].push_fifo(data),
            0x0A4..=0x0A7 => self.direct_sound[1].push_fifo(data),

            0x0A8..=0x0AF => {}

            _ => unreachable!(),
        }
    }

    pub fn write16(&mut self, ctx: &mut impl Context, addr: u32, data: u16) {
        self.write8(ctx, addr, data as u8);
        self.write8(ctx, addr + 1, (data >> 8) as u8);
    }
}
