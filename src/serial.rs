use log::info;
use serde::{Deserialize, Serialize};

#[derive(Default, Serialize, Deserialize)]
pub struct Serial {
    // 00: 9600bps
    // 01: 19200bps
    // 10: 57600bps
    // 11: 115200bps
    baud_rate: u8,

    si_terminal: bool,
    sd_terminal: bool,

    // 00: Master
    // 01: 1st Slave
    // 10: 2nd Slave
    // 11: 3rd Slave
    multi_player_id: u8,

    communication_error: bool,

    // Master
    //   0: No transfer
    //   1: Start transfer
    // Slave
    //   0: Free
    //   1: Busy
    start_bit: bool,

    // 0*: Serial
    // 10: GPIO
    // 11: JOY Bus
    communication_function: u8,

    data_bits: u8,
    io_select: u8, // each bits: 0: Input, 1: Output

    irq_enable: bool,

    pub data: [u16; 4],
    pub data8: u8,
}

impl Serial {
    pub fn read(&mut self, addr: u32) -> Option<u8> {
        Some(match addr {
            // SIODATA
            0x120..=0x127 => {
                let i = (addr - 0x120) as usize;
                info!("Read SIODATA[{i}]");
                0
            }

            // SIOCNT
            0x128 | 0x129 => {
                // TODO
                info!("Read SIOCNT");
                0
            }
            // SIODATA
            0x12A | 0x12B => {
                info!("Read SIODATA8");
                0
            }

            // RCNT
            0x134 => self.data_bits | self.io_select << 4,
            0x135 => self.communication_function << 6 | self.irq_enable as u8,

            // JOY_RECV
            0x150..=0x153 => {
                let i = addr - 0x150;
                info!("Read JOY_RECV[{i}]");
                0
            }
            // JOY_TRANS
            0x154..=0x157 => {
                let i = addr - 0x154;
                info!("Read JOY_TRANS[{i}]");
                0
            }
            // JOYSTAT
            0x158 | 0x159 => {
                info!("Read JOYSTAT");
                0
            }

            _ => return None,
        })
    }

    pub fn write(&mut self, addr: u32, data: u8) {
        match addr {
            // SIODATA
            0x120..=0x127 => {
                let i = (addr - 0x120) as usize;
                info!("SIODATA[{i}] = 0x{data:02X}");
            }

            // SIOCNT
            0x128 | 0x129 => {
                let i = (addr - 0x128) as usize;
                info!("SIOCNT[{i}] = 0x{data:02X}");
            }

            // SIODATA8
            0x12A => self.data8 = data as u8,
            0x12B..=0x12F => {}

            // RCNT
            0x134 => {
                self.data_bits = data & 0x0F;
                self.io_select = (data >> 4) & 0x0F;
            }
            0x135 => {
                self.irq_enable = data & 1 != 0;
                self.communication_function = data >> 6;

                info!(
                    "Set communication function to: {}",
                    match self.communication_function {
                        0 | 1 => "Serial",
                        2 => "GPIO",
                        3 => "JOY Bus",
                        _ => unreachable!(),
                    }
                );
            }

            // JOYCNT
            0x140..=0x14F => {
                let i = addr - 0x140;
                info!("JOYCNT[{i}] = 0x{data:02X}");
            }

            // JOY_RECV
            0x150..=0x153 => {
                let i = addr - 0x150;
                info!("JOY_RECV[{i}] = 0x{data:04X}");
            }
            // JOY_TRANS
            0x154..=0x157 => {
                let i = addr - 0x154;
                info!("JOY_TRANS[{i}] = 0x{data:04X}");
            }
            // JOYSTAT
            0x158 | 0x159 => {
                info!("JOYSTAT = 0x{data:04X}");
            }
            0x15A..=0x15F => {}

            _ => unreachable!("0x{addr:08X}"),
        }
    }
}
