use crate::bus::Bus;

struct Register8(u8);

fn get_r16(high: &Register8, low: &Register8) -> u16 {
    (high.0 as u16) << 8 | low.0 as u16
}

fn set_r16(val: u16, high: &mut Register8, low: &mut Register8) {
    low.0 = val as u8;
    high.0 = (val >> 8) as u8;
}

// for instructions that take r16s, just pass them as the two r8s that make them up
// TODO: maybe instead of allowing instructions to take mutable references to registers, create an
// enum consisting of register symbols, and a method which allows the CPU to convert those to the
// corresponding register.
#[allow(non_camel_case_types)]
enum Instruction<'a> {
    NOP,
    LD_r16_n16(&'a mut Register8, &'a mut Register8),
    LD_r16_r8(&'a Register8, &'a Register8, &'a Register8),
    LD_r8_n8(&'a mut Register8),
    LD_a16_SP,
    LD_r8_r16(&'a mut Register8, &'a Register8, &'a Register8),
    INC_r16(&'a mut Register8, &'a mut Register8),
    INC_r8(&'a mut Register8),
    DEC_r16(&'a mut Register8, &'a mut Register8),
    DEC_r8(&'a mut Register8),
    ADD_HL_r16(&'a Register8, &'a Register8),
    RLCA,
    RRCA,
    NotImplemented,
}

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

    fn decode(&mut self, opcode: u8) -> Instruction {
        match opcode {
            0x00 => Instruction::NOP,
            0x01 => Instruction::LD_r16_n16(&mut self.b, &mut self.c),
            0x02 => Instruction::LD_r16_r8(&self.b, &self.c, &self.a),
            0x03 => Instruction::INC_r16(&mut self.b, &mut self.c),
            0x04 => Instruction::INC_r8(&mut self.b),
            0x05 => Instruction::DEC_r8(&mut self.b),
            0x06 => Instruction::LD_r8_n8(&mut self.b),
            0x07 => Instruction::RLCA,
            0x08 => Instruction::LD_a16_SP,
            0x09 => Instruction::ADD_HL_r16(&self.b, &self.c),
            0x0A => Instruction::LD_r8_r16(&mut self.a, &self.b, &self.c),
            0x0B => Instruction::DEC_r16(&mut self.b, &mut self.c),
            0x0C => Instruction::INC_r8(&mut self.c),
            0x0D => Instruction::DEC_r8(&mut self.c),
            0x0E => Instruction::LD_r8_n8(&mut self.c),
            0x0F => Instruction::RRCA,
            _ => Instruction::NotImplemented,
        }
    }

    // Instructions take a variable amount of CPU cycles
    // from https://emudev.de/gameboy-emulator/opcode-cycles-and-timings/ return the number of
    // cycles executed to develop accurate timing. Yields number of T-states.
    fn execute(&mut self, inst: Instruction) -> u8 {
        match inst {
            Instruction::NOP => 4,

            Instruction::LD_r16_n16(r1, r2) => {
                r2.0 = self.fetch();
                r1.0 = self.fetch();
                12
            }

            Instruction::LD_r16_r8(r16_1, r16_2, r8) => {
                let address = get_r16(r16_1, r16_2) as usize;
                let value = r8.0;
                self.bus.write(address, value);
                8
            }

            Instruction::INC_r16(r1, r2) => {
                let val = get_r16(r1, r2).wrapping_add(1);
                set_r16(val, r1, r2);
                8
            }

            Instruction::INC_r8(r) => {
                self.set_flag_n(false);

                // check overflow from bit 3 (bits 0-3 are on)
                if r.0 & 0x0F == 0x0F {
                    self.set_flag_h(true);
                }

                let new_val = r.0.wrapping_add(1);
                if new_val == 0 {
                    self.set_flag_z(true);
                }

                r.0 = new_val;
                4
            }

            Instruction::DEC_r8(r) => {
                self.set_flag_n(true);

                // check if a borrow from bit 4 is required (bits 0-3 are off)
                if r.0 & 0x0F == 0 {
                    self.set_flag_h(true);
                }

                let new_val = r.0.wrapping_sub(1);
                if new_val == 0 {
                    self.set_flag_z(true);
                }

                r.0 = new_val;
                4
            }

            Instruction::LD_r8_n8(r) => {
                r.0 = self.fetch();
                8
            }

            Instruction::RLCA => {
                self.set_flag_z(false);
                self.set_flag_n(false);
                self.set_flag_h(false);

                self.a.0 = self.a.0.rotate_left(1);

                // set flag c to the leftmost bit that was rotated to the least significant
                // position
                self.set_flag_c(self.a.0 & 1 == 1);
                4
            }

            Instruction::LD_a16_SP => {
                let addr = self.fetch_2();

                // write SP & 0xFF (low byte) to addr, and SP >> 8 (high byte) to addr + 1
                self.bus.write(addr as usize, (self.sp & 0xFF) as u8);
                self.bus.write((addr + 1) as usize, (self.sp >> 8) as u8);
                20
            }

            Instruction::ADD_HL_r16(r1, r2) => {
                self.set_flag_n(false);

                let hl = get_r16(&self.h, &self.l);
                let r16 = get_r16(r1, r2);

                // set flag c on actual overflow
                let (res, c) = hl.overflowing_add(r16);
                if c {
                    self.set_flag_c(true);
                }

                // set flag h if the sum of the bottom 12 bits activate the 13th bit
                if (hl & 0x0FFF) + (r16 & 0x0FFF) > 0x0FFF {
                    self.set_flag_h(true);
                }

                set_r16(res, &mut self.h, &mut self.l);
                8
            }

            Instruction::LD_r8_r16(r8, r16_1, r16_2) => {
                // get value pointed to by r16 and store in r8
                let addr = get_r16(r16_1, r16_2);
                r8.0 = self.bus.read(addr as usize);
                8
            }

            Instruction::DEC_r16(r1, r2) => {
                let val = get_r16(r1, r2).wrapping_sub(1);
                set_r16(val, r1, r2);
                8
            }

            Instruction::RRCA => {
                self.set_flag_z(false);
                self.set_flag_n(false);
                self.set_flag_h(false);

                self.set_flag_c(self.a.0 & 1 == 1);

                self.a.0 = self.a.0.rotate_right(1);
                4
            }

            Instruction::NotImplemented => 4,
        }
    }

    pub fn step(&mut self) {
        let opcode = self.fetch();
        let inst = self.decode(opcode);
        self.execute(inst);
    }
}
