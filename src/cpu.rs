use crate::bus::Bus;

// separate registers into their 8-bit components, but allow methods to read/write as either 8 or
// 16 bit
struct Registers {
    a: u8,
    f: u8,
    b: u8,
    c: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
}

impl Registers {
    fn init() -> Registers {
        Registers {
            a: 0,
            f: 0,
            b: 0,
            c: 0,
            h: 0,
            l: 0,
            sp: 0xFFFE,
            pc: 0x100,
        }
    }

    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }

    fn set_bc(&mut self, val: u16) {
        self.c = val as u8;
        self.b = (val >> 8) as u8;
    }

    fn set_flag_z(&mut self, on: bool) {
        if on {
            self.f |= 0b0000_0001
        } else {
            self.f &= 0b1111_1110
        }
    }

    fn set_flag_n(&mut self, on: bool) {
        if on {
            self.f |= 0b0000_0010
        } else {
            self.f &= 0b1111_1101
        }
    }

    fn set_flag_h(&mut self, on: bool) {
        if on {
            self.f |= 0b0000_0100
        } else {
            self.f &= 0b1111_1011
        }
    }

    fn set_flag_c(&mut self, on: bool) {
        if on {
            self.f |= 0b0000_1000
        } else {
            self.f &= 0b1111_0111
        }
    }

    fn inc_u8(&mut self, reg: u8) -> u8 {
        // check overflow from bit 3 (bits 0-3 are on)
        if reg & 0x0F == 0x0F {
            self.set_flag_h(true);
        }

        let r = reg.wrapping_add(1);
        self.set_flag_n(false);

        if r == 0 {
            self.set_flag_z(true);
        }

        r
    }

    fn dec_u8(&mut self, reg: u8) -> u8 {
        // check if a borrow from bit 4 is required (bits 0-3 are off)
        if reg & 0x0F == 0 {
            self.set_flag_h(true);
        }

        let r = reg.wrapping_sub(1);
        self.set_flag_n(true);

        if r == 0 {
            self.set_flag_z(true);
        }

        r
    }
}

pub struct CPU<'a> {
    registers: Registers,
    bus: &'a mut Bus<'a>,
}

impl CPU<'_> {
    pub fn init<'a>(bus: &'a mut Bus<'a>) -> CPU<'a> {
        CPU {
            registers: Registers::init(),
            bus,
        }
    }

    fn bus_read(&self, address: usize) -> u8 {
        self.bus.read(address)
    }

    fn bus_write(&mut self, address: usize, value: u8) {
        self.bus.write(address, value);
    }

    pub fn fetch(&mut self) -> u8 {
        let byte = self.bus_read(self.registers.pc as usize);
        self.registers.pc += 1;
        byte
    }

    // Instructions take a variable amount of CPU cycles
    // from https://emudev.de/gameboy-emulator/opcode-cycles-and-timings/ return the number of
    // cycles executed to develop accurate timing. Yields number of T-states.
    pub fn execute(&mut self) -> u8 {
        let opcode = self.fetch();
        match opcode {
            // 0x00: NOP
            0x00 => 4,

            // 0x01: LD BC n16
            0x01 => {
                self.registers.c = self.fetch();
                self.registers.b = self.fetch();
                12
            }

            // 0x02: LD [BC] A
            0x02 => {
                let address = self.registers.get_bc() as usize;
                let value = self.registers.a;
                self.bus_write(address, value);
                8
            }

            // 0x03: INC BC
            0x03 => {
                self.registers.set_bc(self.registers.get_bc() + 1);
                8
            }

            // 0x04: INC B
            0x04 => {
                self.registers.b = self.registers.inc_u8(self.registers.b);
                4
            }

            // 0x05: DEC B
            0x05 => {
                self.registers.b = self.registers.dec_u8(self.registers.b);
                4
            }

            // 0x06: LD B n8
            0x06 => {
                self.registers.b = self.fetch();
                8
            }

            // 0x07: RLCA
            0x07 => {
                self.registers.a = self.registers.a.rotate_left(1);

                // set flag c to the leftmost bit that was rotated to the least significant
                // position
                self.registers.set_flag_c(self.registers.a & 1 == 1);
                4
            }

            // 0x08: LD [a16] SP
            0x08 => 20,

            _ => {
                println!("Warning: opcode {:X} not implemented", opcode);
                4
            }
        }
    }
}
