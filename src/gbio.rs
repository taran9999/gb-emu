use crate::timer::Timer;

pub struct Io<'a> {
    serial_data: u8,
    serial_control: u8,
    timer: &'a mut Timer,
}

impl Io<'_> {
    pub fn new<'a>(timer: &'a mut Timer) -> Io {
        Io {
            serial_data: 0,
            serial_control: 0,
            timer,
        }
    }

    pub fn io_read(&self, address: usize) -> u8 {
        match address {
            0xFF01 => self.serial_data,
            0xFF02 => self.serial_control,
            0xFF04..=0xFF07 => self.timer.timer_read(address),
            _ => panic!("io read out of bounds at ${:04X}", address),
        }
    }

    pub fn io_write(&mut self, address: usize, value: u8) {
        match address {
            0xFF01 => self.serial_data = value,
            0xFF02 => self.serial_control = value,
            0xFF04..=0xFF07 => self.timer.timer_write(address, value),
            _ => panic!("io write out of bounds at ${:04X}", address),
        }
    }
}
