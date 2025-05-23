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
        (self.b << 8) as u16 | self.c as u16
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
    // cycles executed to develop accurate timing
    pub fn execute(&mut self) -> u8 {
        let opcode = self.fetch();
        match opcode {
            // 0x00: NOP
            0x00 => 4,

            // 0x01: LD BC n16
            0x01 => {
                self.registers.c = self.fetch();
                self.registers.b = self.fetch();
                3
            }

            // 0x02: LD [BC] A
            0x02 => {
                let address = self.registers.get_bc() as usize;
                let value = self.registers.a;
                self.bus_write(address, value);
                2
            }

            // 0x03: INC BC
            0x03 => {
                self.registers.set_bc(self.registers.get_bc() + 1);
                2
            }

            // 0x04: INC B
            0x04 => {
                let ov;
                (self.registers.b, ov) = self.registers.b.overflowing_add(1);
                if self.registers.b == 0 {
                    self.registers.set_flag_z(true);
                }
                self.registers.set_flag_n(false);
                if ov {
                    self.registers.set_flag_h(true);
                }
                1
            }

            _ => {
                println!("Warning: opcode {:X} not implemented", opcode);
                4
            }
        }
    }
}
