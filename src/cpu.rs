use crate::bus::Bus;

struct Register8(u8);

pub struct CPU<'a> {
    a: Register8,
    f: Register8,
    b: Register8,
    c: Register8,
    h: Register8,
    l: Register8,
    sp: u16,
    pc: u16,
    bus: &'a mut Bus<'a>,
}

impl CPU<'_> {
    pub fn init<'a>(bus: &'a mut Bus<'a>) -> CPU<'a> {
        CPU {
            a: Register8(0),
            f: Register8(0),
            b: Register8(0),
            c: Register8(0),
            h: Register8(0),
            l: Register8(0),
            sp: 0xFFFE,
            pc: 0x100,
            bus,
        }
    }

    fn fetch(&mut self) -> u8 {
        let byte = self.bus.read(self.pc as usize);
        self.pc += 1;
        byte
    }

    // fetch two bytes and return a u16 (little endian)
    fn fetch_2(&mut self) -> u16 {
        let low = self.fetch() as u16;
        let high = self.fetch() as u16;
        (high << 8) | low
    }

    fn set_flag_z(&mut self, on: bool) {
        if on {
            self.f.0 |= 0b0000_0001
        } else {
            self.f.0 &= 0b1111_1110
        }
    }

    fn set_flag_n(&mut self, on: bool) {
        if on {
            self.f.0 |= 0b0000_0010
        } else {
            self.f.0 &= 0b1111_1101
        }
    }

    fn set_flag_h(&mut self, on: bool) {
        if on {
            self.f.0 |= 0b0000_0100
        } else {
            self.f.0 &= 0b1111_1011
        }
    }

    fn set_flag_c(&mut self, on: bool) {
        if on {
            self.f.0 |= 0b0000_1000
        } else {
            self.f.0 &= 0b1111_0111
        }
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
                self.registers.set_flag_z(false);
                self.registers.set_flag_n(false);
                self.registers.set_flag_h(false);

                self.registers.a = self.registers.a.rotate_left(1);

                // set flag c to the leftmost bit that was rotated to the least significant
                // position
                self.registers.set_flag_c(self.registers.a & 1 == 1);
                4
            }

            // 0x08: LD [a16] SP
            0x08 => {
                let addr = self.fetch_2();

                // write SP & 0xFF (low byte) to addr, and SP >> 8 (high byte) to addr + 1
                self.bus_write(addr as usize, (self.registers.sp & 0xFF) as u8);
                self.bus_write((addr + 1) as usize, (self.registers.sp >> 8) as u8);
                20
            }

            // 0x09: ADD HL BC
            0x09 => {
                self.registers.set_flag_n(false);

                let hl = self.registers.get_hl();
                let bc = self.registers.get_bc();

                // set flag c on actual overflow
                let (res, c) = hl.overflowing_add(bc);
                if c {
                    self.registers.set_flag_c(true);
                }

                // set flag h if the sum of the bottom 12 bits activate the 13th bit
                if (hl & 0x0FFF) + (bc & 0x0FFF) > 0x0FFF {
                    self.registers.set_flag_h(true);
                }

                self.registers.set_hl(res);
                8
            }

            // 0x0A: LD A [BC]
            0x0A => {
                // get value pointed to by BC and store in A
                let addr = self.registers.get_bc();
                self.registers.a = self.bus_read(addr as usize);
                8
            }

            // 0x0B: DEC BC
            0x0B => {
                self.registers.set_bc(self.registers.get_bc() - 1);
                8
            }

            // 0x0C: INC C
            0x0C => {
                self.registers.c = self.registers.inc_u8(self.registers.c);
                4
            }

            // 0x0D: DEC C
            0x0D => {
                self.registers.c = self.registers.dec_u8(self.registers.c);
                4
            }

            // 0x0E: LD C n8
            0x0E => {
                self.registers.c = self.fetch();
                8
            }

            // 0x0F: RRCA
            0x0F => {
                self.registers.set_flag_z(false);
                self.registers.set_flag_n(false);
                self.registers.set_flag_h(false);

                self.registers.set_flag_c(self.registers.a & 1 == 1);

                self.registers.a = self.registers.a.rotate_right(1);
                4
            }

            _ => {
                println!("Warning: opcode {:X} not implemented", opcode);
                4
            }
        }
    }
}
