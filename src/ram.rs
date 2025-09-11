pub struct Ram {
    wram: [u8; 0x2000],
    hram: [u8; 0x7F],
}

impl Ram {
    pub fn new() -> Ram {
        Ram {
            wram: [0; 0x2000],
            hram: [0; 0x7F],
        }
    }

    pub fn wram_read(&self, address: usize) -> u8 {
        let addr = address - 0xC000;

        if addr >= 0x2000 {
            panic!("wram read out of bounds at ${:04X}", address);
        } else {
            self.wram[addr]
        }
    }

    pub fn hram_read(&self, address: usize) -> u8 {
        let addr = address - 0xFF80;

        if addr >= 0x7F {
            panic!("hram read out of bounds at ${:04X}", address);
        } else {
            self.hram[addr]
        }
    }

    pub fn wram_write(&mut self, address: usize, value: u8) {
        let addr = address - 0xC000;

        if addr >= 0x2000 {
            panic!("wram write out of bounds at ${:04X}", address);
        } else {
            self.wram[addr] = value;
        }
    }

    pub fn hram_write(&mut self, address: usize, value: u8) {
        let addr = address - 0xFF80;

        if addr >= 0x7F {
            panic!("hram write out of bounds at ${:04X}", address);
        } else {
            self.hram[addr] = value;
        }
    }
}
