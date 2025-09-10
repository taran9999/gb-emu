pub struct Io {
    serial_data: u8,
    serial_control: u8,
}

impl Io {
    pub fn new() -> Io {
        Io {
            serial_data: 0,
            serial_control: 0,
        }
    }

    pub fn io_read(&self, address: usize) -> u8 {
        match address {
            0xFF01 => self.serial_data,
            0xFF02 => self.serial_control,
            _ => panic!("Unsupported read to io at ${:04X}", address),
        }
    }

    pub fn io_write(&mut self, address: usize, value: u8) {
        match address {
            0xFF01 => self.serial_data = value,
            0xFF02 => self.serial_control = value,
            _ => panic!("Unsuppored write to io at ${:04X}", address),
        }
    }
}
