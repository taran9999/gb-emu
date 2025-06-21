use crate::bus::Bus;

struct Register8(u8);

struct Register16<'a>(&'a mut Register8, &'a mut Register8);

impl Register16<'_> {
    fn get(&self) -> u16 {
        (self.0 .0 as u16) << 8 | self.1 .0 as u16
    }

    fn set(&mut self, val: u16) {
        self.1 .0 = val as u8;
        self.0 .0 = (val >> 8) as u8;
    }
}

enum Reg8Symbol {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(PartialEq, Eq)]
enum Reg16Symbol {
    BC,
    DE,
    HL,
    HLI,
    HLD,
}

enum FlagSymbol {
    Z,
    N,
    H,
    C,
}

#[allow(non_camel_case_types)]
enum Instruction {
    NOP,
    LD_r16_n16(Reg16Symbol),
    LD_r16_r8(Reg16Symbol, Reg8Symbol),
    LD_r8_n8(Reg8Symbol),
    LD_a16_SP,
    LD_r8_r16(Reg8Symbol, Reg16Symbol),
    LD_SP_n16,
    LD_HL_n8,
    LD_r8_r8(Reg8Symbol, Reg8Symbol),
    INC_r16(Reg16Symbol),
    INC_r8(Reg8Symbol),
    INC_SP,
    DEC_r16(Reg16Symbol),
    DEC_r8(Reg8Symbol),
    DEC_SP,
    ADD_HL_r16(Reg16Symbol),
    ADD_HL_SP,
    RLCA,
    RRCA,
    RLA,
    RRA,
    STOP,
    JR_n16,
    JR_n16_Conditional(FlagSymbol, bool),
    DAA,
    CPL,
    SCF,
    CCF,
    HALT,
    NotImplemented,
}

pub struct CPU<'a> {
    a: Register8,
    f: Register8,
    b: Register8,
    c: Register8,
    d: Register8,
    e: Register8,
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
            d: Register8(0),
            e: Register8(0),
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

    fn get_flag_z(&self) -> bool {
        self.f.0 & 0b0000_0001 == 0b0000_0001
    }

    fn get_flag_n(&self) -> bool {
        self.f.0 & 0b0000_0010 == 0b0000_0010
    }

    fn get_flag_h(&self) -> bool {
        self.f.0 & 0b0000_0100 == 0b0000_0100
    }

    fn get_flag_c(&self) -> bool {
        self.f.0 & 0b0000_1000 == 0b0000_1000
    }

    fn set_flag(&mut self, flag: &FlagSymbol, on: bool) {
        match flag {
            FlagSymbol::Z => self.set_flag_z(on),
            FlagSymbol::N => self.set_flag_n(on),
            FlagSymbol::H => self.set_flag_h(on),
            FlagSymbol::C => self.set_flag_c(on),
        }
    }

    fn get_flag(&self, flag: &FlagSymbol) -> bool {
        match flag {
            FlagSymbol::Z => self.get_flag_z(),
            FlagSymbol::N => self.get_flag_n(),
            FlagSymbol::H => self.get_flag_h(),
            FlagSymbol::C => self.get_flag_c(),
        }
    }

    fn reg8_from_symbol(&mut self, sym: &Reg8Symbol) -> &mut Register8 {
        match sym {
            Reg8Symbol::A => &mut self.a,
            Reg8Symbol::F => &mut self.f,
            Reg8Symbol::B => &mut self.b,
            Reg8Symbol::C => &mut self.c,
            Reg8Symbol::D => &mut self.d,
            Reg8Symbol::E => &mut self.e,
            Reg8Symbol::H => &mut self.h,
            Reg8Symbol::L => &mut self.l,
        }
    }

    fn reg16_from_symbol(&mut self, sym: &Reg16Symbol) -> Register16 {
        match sym {
            Reg16Symbol::BC => Register16(&mut self.b, &mut self.c),
            Reg16Symbol::DE => Register16(&mut self.d, &mut self.e),
            Reg16Symbol::HL | Reg16Symbol::HLI | Reg16Symbol::HLD => {
                Register16(&mut self.h, &mut self.l)
            }
        }
    }

    fn inc_r16(&mut self, r16s: Reg16Symbol) -> u8 {
        let mut r16 = self.reg16_from_symbol(&r16s);
        let val = r16.get().wrapping_add(1);
        r16.set(val);
        8
    }

    fn dec_r16(&mut self, r16s: Reg16Symbol) -> u8 {
        let mut r16 = self.reg16_from_symbol(&r16s);
        let val = r16.get().wrapping_sub(1);
        r16.set(val);
        8
    }

    fn sum_u16_with_flags(&mut self, n1: u16, n2: u16) -> u16 {
        // set flag c on actual overflow
        let (res, c) = n1.overflowing_add(n2);
        if c {
            self.set_flag_c(true);
        }

        // set flag h if the sum of the bottom 12 bits activate the 13th bit
        if (n1 & 0x0FFF) + (n2 & 0x0FFF) > 0x0FFF {
            self.set_flag_h(true);
        }

        res
    }

    fn decode(&mut self, opcode: u8) -> Instruction {
        match opcode {
            0x00 => Instruction::NOP,
            0x01 => Instruction::LD_r16_n16(Reg16Symbol::BC),
            0x02 => Instruction::LD_r16_r8(Reg16Symbol::BC, Reg8Symbol::A),
            0x03 => Instruction::INC_r16(Reg16Symbol::BC),
            0x04 => Instruction::INC_r8(Reg8Symbol::B),
            0x05 => Instruction::DEC_r8(Reg8Symbol::B),
            0x06 => Instruction::LD_r8_n8(Reg8Symbol::B),
            0x07 => Instruction::RLCA,
            0x08 => Instruction::LD_a16_SP,
            0x09 => Instruction::ADD_HL_r16(Reg16Symbol::BC),
            0x0A => Instruction::LD_r8_r16(Reg8Symbol::A, Reg16Symbol::BC),
            0x0B => Instruction::DEC_r16(Reg16Symbol::BC),
            0x0C => Instruction::INC_r8(Reg8Symbol::C),
            0x0D => Instruction::DEC_r8(Reg8Symbol::C),
            0x0E => Instruction::LD_r8_n8(Reg8Symbol::C),
            0x0F => Instruction::RRCA,
            0x10 => Instruction::STOP,
            0x11 => Instruction::LD_r16_n16(Reg16Symbol::DE),
            0x12 => Instruction::LD_r16_r8(Reg16Symbol::DE, Reg8Symbol::A),
            0x13 => Instruction::INC_r16(Reg16Symbol::DE),
            0x14 => Instruction::INC_r8(Reg8Symbol::D),
            0x15 => Instruction::DEC_r8(Reg8Symbol::D),
            0x16 => Instruction::LD_r8_n8(Reg8Symbol::D),
            0x17 => Instruction::RLA,
            0x18 => Instruction::JR_n16,
            0x19 => Instruction::ADD_HL_r16(Reg16Symbol::DE),
            0x1A => Instruction::LD_r8_r16(Reg8Symbol::A, Reg16Symbol::DE),
            0x1B => Instruction::DEC_r16(Reg16Symbol::DE),
            0x1C => Instruction::INC_r8(Reg8Symbol::E),
            0x1D => Instruction::DEC_r8(Reg8Symbol::E),
            0x1E => Instruction::LD_r8_n8(Reg8Symbol::E),
            0x1F => Instruction::RRA,
            0x20 => Instruction::JR_n16_Conditional(FlagSymbol::Z, false),
            0x21 => Instruction::LD_r16_n16(Reg16Symbol::DE),
            0x22 => Instruction::LD_r16_r8(Reg16Symbol::HLI, Reg8Symbol::A),
            0x23 => Instruction::INC_r16(Reg16Symbol::HL),
            0x24 => Instruction::INC_r8(Reg8Symbol::H),
            0x25 => Instruction::DEC_r8(Reg8Symbol::H),
            0x26 => Instruction::LD_r8_n8(Reg8Symbol::H),
            0x27 => Instruction::DAA,
            0x28 => Instruction::JR_n16_Conditional(FlagSymbol::Z, true),
            0x29 => Instruction::ADD_HL_r16(Reg16Symbol::HL),
            0x2A => Instruction::LD_r8_r16(Reg8Symbol::A, Reg16Symbol::HLI),
            0x2B => Instruction::DEC_r16(Reg16Symbol::HL),
            0x2C => Instruction::INC_r8(Reg8Symbol::L),
            0x2D => Instruction::DEC_r8(Reg8Symbol::L),
            0x2E => Instruction::LD_r8_n8(Reg8Symbol::L),
            0x2F => Instruction::CPL,
            0x30 => Instruction::JR_n16_Conditional(FlagSymbol::C, false),
            0x31 => Instruction::LD_SP_n16,
            0x32 => Instruction::LD_r16_r8(Reg16Symbol::HLD, Reg8Symbol::A),
            0x33 => Instruction::INC_SP,
            0x34 => Instruction::INC_r16(Reg16Symbol::HL),
            0x35 => Instruction::DEC_r16(Reg16Symbol::HL),
            0x36 => Instruction::LD_HL_n8,
            0x37 => Instruction::SCF,
            0x38 => Instruction::JR_n16_Conditional(FlagSymbol::C, true),
            0x39 => Instruction::ADD_HL_SP,
            0x3A => Instruction::LD_r8_r16(Reg8Symbol::A, Reg16Symbol::HLD),
            0x3B => Instruction::DEC_SP,
            0x3C => Instruction::INC_r8(Reg8Symbol::A),
            0x3D => Instruction::DEC_r8(Reg8Symbol::A),
            0x3E => Instruction::LD_r8_n8(Reg8Symbol::A),
            0x3F => Instruction::CCF,
            0x40 => Instruction::LD_r8_r8(Reg8Symbol::B, Reg8Symbol::B),
            0x41 => Instruction::LD_r8_r8(Reg8Symbol::B, Reg8Symbol::C),
            0x42 => Instruction::LD_r8_r8(Reg8Symbol::B, Reg8Symbol::D),
            0x43 => Instruction::LD_r8_r8(Reg8Symbol::B, Reg8Symbol::E),
            0x44 => Instruction::LD_r8_r8(Reg8Symbol::B, Reg8Symbol::H),
            0x45 => Instruction::LD_r8_r8(Reg8Symbol::B, Reg8Symbol::L),
            0x46 => Instruction::LD_r8_r16(Reg8Symbol::B, Reg16Symbol::HL),
            0x47 => Instruction::LD_r8_r8(Reg8Symbol::B, Reg8Symbol::A),
            0x48 => Instruction::LD_r8_r8(Reg8Symbol::C, Reg8Symbol::B),
            0x49 => Instruction::LD_r8_r8(Reg8Symbol::C, Reg8Symbol::C),
            0x4A => Instruction::LD_r8_r8(Reg8Symbol::C, Reg8Symbol::D),
            0x4B => Instruction::LD_r8_r8(Reg8Symbol::C, Reg8Symbol::E),
            0x4C => Instruction::LD_r8_r8(Reg8Symbol::C, Reg8Symbol::H),
            0x4D => Instruction::LD_r8_r8(Reg8Symbol::C, Reg8Symbol::L),
            0x4E => Instruction::LD_r8_r16(Reg8Symbol::C, Reg16Symbol::HL),
            0x4F => Instruction::LD_r8_r8(Reg8Symbol::C, Reg8Symbol::A),
            0x50 => Instruction::LD_r8_r8(Reg8Symbol::D, Reg8Symbol::B),
            0x51 => Instruction::LD_r8_r8(Reg8Symbol::D, Reg8Symbol::C),
            0x52 => Instruction::LD_r8_r8(Reg8Symbol::D, Reg8Symbol::D),
            0x53 => Instruction::LD_r8_r8(Reg8Symbol::D, Reg8Symbol::E),
            0x54 => Instruction::LD_r8_r8(Reg8Symbol::D, Reg8Symbol::H),
            0x55 => Instruction::LD_r8_r8(Reg8Symbol::D, Reg8Symbol::L),
            0x56 => Instruction::LD_r8_r16(Reg8Symbol::D, Reg16Symbol::HL),
            0x57 => Instruction::LD_r8_r8(Reg8Symbol::D, Reg8Symbol::A),
            0x58 => Instruction::LD_r8_r8(Reg8Symbol::E, Reg8Symbol::B),
            0x59 => Instruction::LD_r8_r8(Reg8Symbol::E, Reg8Symbol::C),
            0x5A => Instruction::LD_r8_r8(Reg8Symbol::E, Reg8Symbol::D),
            0x5B => Instruction::LD_r8_r8(Reg8Symbol::E, Reg8Symbol::E),
            0x5C => Instruction::LD_r8_r8(Reg8Symbol::E, Reg8Symbol::H),
            0x5D => Instruction::LD_r8_r8(Reg8Symbol::E, Reg8Symbol::L),
            0x5E => Instruction::LD_r8_r16(Reg8Symbol::E, Reg16Symbol::HL),
            0x5F => Instruction::LD_r8_r8(Reg8Symbol::E, Reg8Symbol::A),
            0x60 => Instruction::LD_r8_r8(Reg8Symbol::H, Reg8Symbol::B),
            0x61 => Instruction::LD_r8_r8(Reg8Symbol::H, Reg8Symbol::C),
            0x62 => Instruction::LD_r8_r8(Reg8Symbol::H, Reg8Symbol::D),
            0x63 => Instruction::LD_r8_r8(Reg8Symbol::H, Reg8Symbol::E),
            0x64 => Instruction::LD_r8_r8(Reg8Symbol::H, Reg8Symbol::H),
            0x65 => Instruction::LD_r8_r8(Reg8Symbol::H, Reg8Symbol::L),
            0x66 => Instruction::LD_r8_r16(Reg8Symbol::H, Reg16Symbol::HL),
            0x67 => Instruction::LD_r8_r8(Reg8Symbol::H, Reg8Symbol::A),
            0x68 => Instruction::LD_r8_r8(Reg8Symbol::L, Reg8Symbol::B),
            0x69 => Instruction::LD_r8_r8(Reg8Symbol::L, Reg8Symbol::C),
            0x6A => Instruction::LD_r8_r8(Reg8Symbol::L, Reg8Symbol::D),
            0x6B => Instruction::LD_r8_r8(Reg8Symbol::L, Reg8Symbol::E),
            0x6C => Instruction::LD_r8_r8(Reg8Symbol::L, Reg8Symbol::H),
            0x6D => Instruction::LD_r8_r8(Reg8Symbol::L, Reg8Symbol::L),
            0x6E => Instruction::LD_r8_r16(Reg8Symbol::L, Reg16Symbol::HL),
            0x6F => Instruction::LD_r8_r8(Reg8Symbol::L, Reg8Symbol::A),
            0x70 => Instruction::LD_r16_r8(Reg16Symbol::HL, Reg8Symbol::B),
            0x71 => Instruction::LD_r16_r8(Reg16Symbol::HL, Reg8Symbol::C),
            0x72 => Instruction::LD_r16_r8(Reg16Symbol::HL, Reg8Symbol::D),
            0x73 => Instruction::LD_r16_r8(Reg16Symbol::HL, Reg8Symbol::E),
            0x74 => Instruction::LD_r16_r8(Reg16Symbol::HL, Reg8Symbol::H),
            0x75 => Instruction::LD_r16_r8(Reg16Symbol::HL, Reg8Symbol::L),
            0x76 => Instruction::HALT,
            0x77 => Instruction::LD_r16_r8(Reg16Symbol::HL, Reg8Symbol::A),
            0x78 => Instruction::LD_r8_r8(Reg8Symbol::A, Reg8Symbol::B),
            0x79 => Instruction::LD_r8_r8(Reg8Symbol::A, Reg8Symbol::C),
            0x7A => Instruction::LD_r8_r8(Reg8Symbol::A, Reg8Symbol::D),
            0x7B => Instruction::LD_r8_r8(Reg8Symbol::A, Reg8Symbol::E),
            0x7C => Instruction::LD_r8_r8(Reg8Symbol::A, Reg8Symbol::H),
            0x7D => Instruction::LD_r8_r8(Reg8Symbol::A, Reg8Symbol::L),
            0x7E => Instruction::LD_r8_r16(Reg8Symbol::A, Reg16Symbol::HL),
            0x7F => Instruction::LD_r8_r8(Reg8Symbol::A, Reg8Symbol::A),

            _ => {
                println!("Warning: no implementation for opcode {:X}", opcode);
                Instruction::NotImplemented
            }
        }
    }

    // Instructions take a variable amount of CPU cycles
    // from https://emudev.de/gameboy-emulator/opcode-cycles-and-timings/ return the number of
    // cycles executed to develop accurate timing. Yields number of T-states.
    fn execute(&mut self, inst: Instruction) -> u8 {
        match inst {
            Instruction::NOP => 4,

            Instruction::LD_r16_n16(r16s) => {
                let val = self.fetch_2();
                let mut r16 = self.reg16_from_symbol(&r16s);
                r16.set(val);
                12
            }

            Instruction::LD_r16_r8(r16s, r8s) => {
                let r16 = self.reg16_from_symbol(&r16s);
                let address = r16.get() as usize;

                let r8 = self.reg8_from_symbol(&r8s);
                let value = r8.0;
                self.bus.write(address, value);

                if r16s == Reg16Symbol::HLI {
                    self.inc_r16(r16s);
                } else if r16s == Reg16Symbol::HLD {
                    self.dec_r16(r16s);
                }

                8
            }

            Instruction::INC_r16(r16s) => self.inc_r16(r16s),

            Instruction::INC_r8(r8s) => {
                let r = self.reg8_from_symbol(&r8s).0;

                self.set_flag_n(false);

                // check overflow from bit 3 (bits 0-3 are on)
                if r & 0x0F == 0x0F {
                    self.set_flag_h(true);
                }

                let new_val = r.wrapping_add(1);
                if new_val == 0 {
                    self.set_flag_z(true);
                }

                self.reg8_from_symbol(&r8s).0 = r;
                4
            }

            Instruction::DEC_r8(r8s) => {
                let r = self.reg8_from_symbol(&r8s).0;

                self.set_flag_n(true);

                // check if a borrow from bit 4 is required (bits 0-3 are off)
                if r & 0x0F == 0 {
                    self.set_flag_h(true);
                }

                let new_val = r.wrapping_sub(1);
                if new_val == 0 {
                    self.set_flag_z(true);
                }

                self.reg8_from_symbol(&r8s).0 = r;
                4
            }

            Instruction::LD_r8_n8(r8s) => {
                let val = self.fetch();
                self.reg8_from_symbol(&r8s).0 = val;
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

            Instruction::ADD_HL_r16(r16s) => {
                self.set_flag_n(false);

                let hl = self.reg16_from_symbol(&Reg16Symbol::HL).get();
                let r16_val = self.reg16_from_symbol(&r16s).get();

                let res = self.sum_u16_with_flags(hl, r16_val);

                self.reg16_from_symbol(&Reg16Symbol::HL).set(res);
                8
            }

            Instruction::LD_r8_r16(r8s, r16s) => {
                // get value pointed to by r16 and store in r8
                let addr = self.reg16_from_symbol(&r16s).get() as usize;
                let val = self.bus.read(addr as usize);
                self.reg8_from_symbol(&r8s).0 = val;

                if r16s == Reg16Symbol::HLI {
                    self.inc_r16(r16s);
                } else if r16s == Reg16Symbol::HLD {
                    self.dec_r16(r16s);
                }

                8
            }

            Instruction::DEC_r16(r16s) => self.dec_r16(r16s),

            Instruction::RRCA => {
                self.set_flag_z(false);
                self.set_flag_n(false);
                self.set_flag_h(false);

                self.set_flag_c(self.a.0 & 1 == 1);

                self.a.0 = self.a.0.rotate_right(1);
                4
            }

            Instruction::RLA => {
                self.set_flag_z(false);
                self.set_flag_n(false);
                self.set_flag_h(false);

                // shift left, then set the last bit to current value of c flag, and set c flag to
                // the shifted out bit
                let left_bit = self.a.0 & 0b1000_0000 == 0b1000_0000;
                self.a.0 = self.a.0 << 1;

                if self.get_flag_c() {
                    self.a.0 |= 0b0000_0001;
                } else {
                    self.a.0 &= 0b1111_1110;
                }

                self.set_flag_c(left_bit);
                4
            }

            Instruction::RRA => {
                self.set_flag_z(false);
                self.set_flag_n(false);
                self.set_flag_h(false);

                let right_bit = self.a.0 & 0b0000_0001 == 0b0000_0001;
                self.a.0 = self.a.0 >> 1;

                if self.get_flag_c() {
                    self.a.0 |= 0b1000_0000;
                } else {
                    self.a.0 &= 0b0111_1111;
                }

                self.set_flag_c(right_bit);
                4
            }

            Instruction::JR_n16 => {
                let ofs = self.fetch() as i8;
                let addr = self.fetch_2();
                self.pc = addr.wrapping_add_signed(ofs.into()) + 2;
                12
            }

            Instruction::JR_n16_Conditional(fls, on) => {
                let ofs = self.fetch() as i8;
                let flag = self.get_flag(&fls);
                if flag == on {
                    let addr = self.fetch_2();
                    self.pc = addr.wrapping_add_signed(ofs.into()) + 2;
                    12
                } else {
                    8
                }
            }

            Instruction::STOP => {
                todo!();
            }

            Instruction::DAA => {
                let mut adj = 0;
                self.set_flag_h(false);

                if self.get_flag_n() {
                    if self.get_flag_h() {
                        adj += 0x06;
                    }

                    if self.get_flag_c() {
                        adj += 0x60;
                    }

                    self.a.0 -= adj;
                } else {
                    if self.get_flag_h() || self.a.0 & 0x0F > 0x09 {
                        adj += 0x06;
                    }

                    if self.get_flag_c() || self.a.0 > 0x99 {
                        adj += 0x60;
                        self.set_flag_c(true);
                    }

                    self.a.0 += adj;
                }

                if self.a.0 == 0 {
                    self.set_flag_z(true);
                }

                4
            }

            Instruction::CPL => {
                self.set_flag_n(true);
                self.set_flag_h(true);

                self.a.0 = !self.a.0;
                4
            }

            Instruction::LD_SP_n16 => {
                let addr = self.fetch_2();
                self.sp = addr;
                12
            }

            Instruction::INC_SP => {
                self.sp = self.sp.wrapping_add(1);
                8
            }

            Instruction::LD_HL_n8 => {
                let value = self.fetch();
                let hl = self.reg16_from_symbol(&Reg16Symbol::HL);
                let address = hl.get() as usize;
                self.bus.write(address, value);
                12
            }

            Instruction::SCF => {
                self.set_flag_n(false);
                self.set_flag_h(false);
                self.set_flag_c(true);
                4
            }

            Instruction::ADD_HL_SP => {
                self.set_flag_n(false);

                let hl = self.reg16_from_symbol(&Reg16Symbol::HL).get();

                let res = self.sum_u16_with_flags(hl, self.sp);
                self.reg16_from_symbol(&Reg16Symbol::HL).set(res);
                8
            }

            Instruction::DEC_SP => {
                self.sp = self.sp.wrapping_sub(1);
                8
            }

            Instruction::CCF => {
                self.set_flag_n(false);
                self.set_flag_h(false);
                self.set_flag_c(!self.get_flag_c());
                4
            }

            Instruction::LD_r8_r8(r8s1, r8s2) => {
                // copy value at r82 to r81
                let r82 = self.reg8_from_symbol(&r8s2);
                let val = r82.0;
                let r81 = self.reg8_from_symbol(&r8s1);
                r81.0 = val;
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
