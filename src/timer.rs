use crate::interrupts::Interrupt;

pub struct Timer {
    div: u16,
    tima: u8,
    tma: u8,
    tac: u8,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            div: 0xAC00,
            tima: 0,
            tma: 0,
            tac: 0,
        }
    }

    pub fn tick(&mut self) -> Option<Interrupt> {
        // clock select
        let update = match self.tac & 0b11 {
            0b00 => self.div % 256 == 0,
            0b01 => self.div % 4 == 0,
            0b10 => self.div % 16 == 0,
            0b11 => self.div % 64 == 0,
            _ => false,
        };

        self.div = self.div.wrapping_add(1);

        // check enable bit of tac
        if update && (self.tac >> 2) & 1 == 1 {
            self.tima += 1;
            if self.tima == 0xFF {
                self.tima = self.tma;

                // send timer interrupt
                return Some(Interrupt::Timer);
            }
        }

        None
    }

    pub fn timer_read(&self, addr: usize) -> u8 {
        match addr {
            0xFF04 => (self.div >> 8) as u8,
            0xFF05 => self.tima,
            0xFF06 => self.tma,
            0xFF07 => self.tac,
            _ => panic!("timer read out of bounds at ${:04X}", addr),
        }
    }

    pub fn timer_write(&mut self, addr: usize, val: u8) {
        match addr {
            0xFF04 => self.div = 0,
            0xFF05 => self.tima = val,
            0xFF06 => self.tma = val,
            0xFF07 => self.tac = val,
            _ => panic!("timer write out of bounds at ${:04X}", addr),
        }
    }
}
