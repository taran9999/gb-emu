use crate::apu::APU;
use crate::interrupts::Interrupt;
use crate::timer::Timer;

pub struct Io<'a> {
    serial_data: u8,
    serial_control: u8,
    timer: &'a mut Timer,
    apu: &'a mut APU,
    int_flag: u8,
    int_enable_flag: u8,
}

impl Io<'_> {
    pub fn new<'a>(timer: &'a mut Timer, apu: &'a mut APU) -> Io<'a> {
        Io {
            serial_data: 0,
            serial_control: 0,
            timer,
            apu,
            int_flag: 0,
            int_enable_flag: 0,
        }
    }

    pub fn io_read(&self, address: usize) -> u8 {
        match address {
            0xFF01 => self.serial_data,
            0xFF02 => self.serial_control,
            0xFF04..=0xFF07 => self.timer.timer_read(address),
            0xFF0F => self.int_flag,
            0xFF10..=0xFF3F => self.apu.apu_read(address),
            0xFFFF => self.int_enable_flag,
            _ => panic!("io read out of bounds at ${:04X}", address),
        }
    }

    pub fn io_write(&mut self, address: usize, value: u8) {
        match address {
            0xFF01 => self.serial_data = value,
            0xFF02 => self.serial_control = value,
            0xFF04..=0xFF07 => self.timer.timer_write(address, value),
            0xFF0F => self.int_flag = value,
            0xFF10..=0xFF3F => self.apu.apu_write(address, value),
            0xFFFF => self.int_enable_flag = value,
            _ => println!("(Warning) io write out of bounds at ${:04X}", address),
        }
    }

    pub fn timer_tick(&mut self) {
        if let Some(Interrupt::Timer) = self.timer.tick() {
            self.int_flag |= Interrupt::Timer.bit_enable();
        }
    }
}
