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

    irq_enable: bool,

    pub data: [u16; 4],
    pub data8: u8,
}
