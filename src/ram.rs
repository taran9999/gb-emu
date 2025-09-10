pub struct Ram {
    wram: [u8; 0x2000],
}

impl Ram {
    pub fn new() -> Ram {
        Ram { wram: [0; 0x2000] }
    }

    pub fn wram_read(&self, address: usize) -> u8 {
        let addr = address - 0xC000;

        if addr >= 0x2000 {
            panic!("wram read out of bounds at ${:04X}", address);
        } else {
            self.wram[addr]
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
}
