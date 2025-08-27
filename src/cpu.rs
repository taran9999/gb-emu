use std::cell::Cell;

use crate::bus::Bus;

#[cfg(test)]
mod tests;

struct Flags {
    z: bool,
    n: bool,
    h: bool,
    c: bool,
}

impl Flags {
    fn new() -> Flags {
        Flags {
            z: false,
            n: false,
            h: false,
            c: false,
        }
    }

    fn to_u8(&self) -> u8 {
        let mut res: u8 = 0;
        if self.z {
            res |= 0b1;
        }

        if self.n {
            res |= 0b10;
        }

        if self.h {
            res |= 0b100;
        }

        if self.c {
            res |= 0b1000;
        }

        res
    }

    fn set_from_u8(&mut self, val: u8) {
        self.z = val & 1 == 1;
        self.n = (val >> 1) & 1 == 1;
        self.h = (val >> 2) & 1 == 1;
        self.c = (val >> 3) & 1 == 1;
    }
}

enum Reg8Symbol {
    A,
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
    C,
}

#[allow(non_camel_case_types)]
enum Instruction {
    NOP,

    LD_r8_r8(Reg8Symbol, Reg8Symbol),
    LD_r8_n8(Reg8Symbol),
    LD_r8_r16(Reg8Symbol, Reg16Symbol),
    LD_r16_r8(Reg16Symbol, Reg8Symbol),
    LD_r16_n16(Reg16Symbol),
    LD_a16_SP,
    LD_SP_n16,
    LD_SP_HL,
    LD_a16_A,
    LD_A_a16,
    LD_HL_n8,
    LD_HL_SP_e8,
    LDH_n8_A,
    LDH_A_n8,
    LDH_C_A,
    LDH_A_C,

    INC_r16(Reg16Symbol),
    INC_r8(Reg8Symbol),
    INC_SP,

    DEC_r16(Reg16Symbol),
    DEC_r8(Reg8Symbol),
    DEC_SP,

    ADD_HL_r16(Reg16Symbol),
    ADD_HL_SP,
    ADD_A_r8(Reg8Symbol),
    ADD_A_HL,
    ADD_A_n8,
    ADD_SP_e8,

    ADC_A_r8(Reg8Symbol),
    ADC_A_HL,
    ADC_A_n8,

    SUB_A_r8(Reg8Symbol),
    SUB_A_HL,
    SUB_A_n8,

    SBC_A_r8(Reg8Symbol),
    SBC_A_HL,
    SBC_A_n8,

    AND_A_r8(Reg8Symbol),
    AND_A_HL,
    AND_A_n8,

    XOR_A_r8(Reg8Symbol),
    XOR_A_HL,
    XOR_A_n8,

    OR_A_r8(Reg8Symbol),
    OR_A_HL,
    OR_A_n8,

    CP_A_r8(Reg8Symbol),
    CP_A_HL,
    CP_A_n8,

    JP_n16,
    JP_n16_Conditional(FlagSymbol, bool),
    JP_HL,

    JR_n16,
    JR_n16_Conditional(FlagSymbol, bool),

    RET,
    RET_Conditional(FlagSymbol, bool),
    RETI,

    PUSH_r16(Reg16Symbol),
    POP_r16(Reg16Symbol),
    PUSH_AF,
    POP_AF,

    CALL_n16,
    CALL_n16_Conditional(FlagSymbol, bool),
    RST(u16),

    RLCA,
    RRCA,
    RLA,
    RRA,
    DAA,
    CPL,
    SCF,
    CCF,
    DI,
    EI,
    STOP,
    HALT,

    PREFIX,
    RLC(Reg8Symbol),
    RLC_HL,
    RL(Reg8Symbol),
    RL_HL,
    RRC(Reg8Symbol),
    RRC_HL,
    RR(Reg8Symbol),
    RR_HL,

    SLA(Reg8Symbol),
    SLA_HL,
    SRA(Reg8Symbol),
    SRA_HL,

    SWAP(Reg8Symbol),
    SWAP_HL,

    SRL(Reg8Symbol),
    SRL_HL,

    // use u8 as u3 - values should range from 0 to 7
    BIT(u8, Reg8Symbol),
    BIT_HL(u8),
    RES(u8, Reg8Symbol),
    RES_HL(u8),
    SET(u8, Reg8Symbol),
    SET_HL(u8),

    NotImplemented,
}

pub struct CPU<'a> {
    a: Cell<u8>,
    b: Cell<u8>,
    c: Cell<u8>,
    d: Cell<u8>,
    e: Cell<u8>,
    h: Cell<u8>,
    l: Cell<u8>,
    f: Flags,
    sp: u16,
    pc: u16,
    bus: &'a mut Bus<'a>,
}

impl CPU<'_> {
    pub fn init<'a>(bus: &'a mut Bus<'a>) -> CPU<'a> {
        CPU {
            a: Cell::new(0),
            b: Cell::new(0),
            c: Cell::new(0),
            d: Cell::new(0),
            e: Cell::new(0),
            h: Cell::new(0),
            l: Cell::new(0),
            f: Flags::new(),
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

    fn get_r8(&self, sym: &Reg8Symbol) -> u8 {
        match sym {
            Reg8Symbol::A => self.a.get(),
            Reg8Symbol::B => self.b.get(),
            Reg8Symbol::C => self.c.get(),
            Reg8Symbol::D => self.d.get(),
            Reg8Symbol::E => self.e.get(),
            Reg8Symbol::H => self.h.get(),
            Reg8Symbol::L => self.l.get(),
        }
    }

    fn set_r8(&self, sym: &Reg8Symbol, val: u8) {
        match sym {
            Reg8Symbol::A => self.a.set(val),
            Reg8Symbol::B => self.b.set(val),
            Reg8Symbol::C => self.c.set(val),
            Reg8Symbol::D => self.d.set(val),
            Reg8Symbol::E => self.e.set(val),
            Reg8Symbol::H => self.h.set(val),
            Reg8Symbol::L => self.l.set(val),
        }
    }

    fn get_r16(&self, sym: &Reg16Symbol) -> u16 {
        let (high, low) = match sym {
            Reg16Symbol::BC => (self.get_r8(&Reg8Symbol::B), self.get_r8(&Reg8Symbol::C)),
            Reg16Symbol::DE => (self.get_r8(&Reg8Symbol::D), self.get_r8(&Reg8Symbol::E)),
            Reg16Symbol::HL | Reg16Symbol::HLI | Reg16Symbol::HLD => {
                (self.get_r8(&Reg8Symbol::H), self.get_r8(&Reg8Symbol::L))
            }
        };

        (high as u16) << 8 | low as u16
    }

    fn set_r16(&self, sym: &Reg16Symbol, val: u16) {
        let (high, low) = ((val >> 8) as u8, val as u8);

        match sym {
            Reg16Symbol::BC => {
                self.set_r8(&Reg8Symbol::B, high);
                self.set_r8(&Reg8Symbol::C, low);
            }

            Reg16Symbol::DE => {
                self.set_r8(&Reg8Symbol::D, high);
                self.set_r8(&Reg8Symbol::E, low);
            }

            Reg16Symbol::HL | Reg16Symbol::HLI | Reg16Symbol::HLD => {
                self.set_r8(&Reg8Symbol::H, high);
                self.set_r8(&Reg8Symbol::L, low);
            }
        }
    }

    fn inc_r16(&mut self, r16s: Reg16Symbol) -> u8 {
        self.set_r16(&r16s, self.get_r16(&r16s).wrapping_add(1));
        8
    }

    fn dec_r16(&mut self, r16s: Reg16Symbol) -> u8 {
        self.set_r16(&r16s, self.get_r16(&r16s).wrapping_sub(1));
        8
    }

    fn sum_u16_with_flags(&mut self, n1: u16, n2: u16) -> u16 {
        // set flag c on actual overflow
        let (res, c) = n1.overflowing_add(n2);
        self.f.c = c;

        // set flag h if the sum of the bottom 12 bits activate the 13th bit
        self.f.h = (n1 & 0xFFF) as u32 + (n2 & 0xFFF) as u32 > 0xFFF;

        res
    }

    fn sum_u8_with_flags(&mut self, n1: u8, n2: u8) -> u8 {
        // overflow from bit 3
        self.f.h = (n1 & 0xF) as u16 + (n2 & 0xF) as u16 > 0xF;

        // overflow from bit 7
        let (res, c) = n1.overflowing_add(n2);
        self.f.c = c;

        res
    }

    fn sub_u8_with_flags(&mut self, n1: u8, n2: u8) -> u8 {
        let res = n1.wrapping_sub(n2);

        // borrow from bit 4
        self.f.h = (n1 & 0x0F) < (n2 & 0x0F);

        // full borrow
        self.f.c = n1 < n2;

        res
    }

    fn stack_push_u8(&mut self, val: u8) {
        self.sp = self.sp.wrapping_sub(1);
        self.bus.write(self.sp as usize, val);
    }

    fn stack_push_u16(&mut self, val: u16) {
        let low = val as u8;
        let high = (val >> 8) as u8;
        self.stack_push_u8(high);
        self.stack_push_u8(low);
    }

    fn stack_pop_u8(&mut self) -> u8 {
        let val = self.bus.read(self.sp as usize);
        self.sp = self.sp.wrapping_add(1);
        val
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let low = self.stack_pop_u8();
        let high = self.stack_pop_u8();
        (high as u16) << 8 | low as u16
    }

    fn u8_rot_left(&mut self, val: u8) -> u8 {
        self.f.c = false;
        self.f.n = false;
        self.f.h = false;

        let res = val.rotate_left(1);

        // set flag c to the leftmost bit that was rotated to the least significant
        // position
        self.f.c = val & 1 == 1;

        res
    }

    fn u8_rot_left_through_carry(&mut self, val: u8) -> u8 {
        self.f.z = false;
        self.f.n = false;
        self.f.h = false;

        // shift left, then set the last bit to current value of c flag, and set c flag to
        // the shifted out bit
        let left_bit = val & 0b1000_0000 == 0b1000_0000;
        let mut res = val << 1;

        if self.f.c {
            res |= 0b0000_0001;
        } else {
            res &= 0b1111_1110;
        }

        self.f.c = left_bit;

        res
    }

    fn u8_rot_right_through_carry(&mut self, val: u8) -> u8 {
        self.f.z = false;
        self.f.n = false;
        self.f.h = false;

        let right_bit = val & 0b0000_0001 == 0b0000_0001;
        let mut res = val >> 1;

        if self.f.c {
            res |= 0b1000_0000;
        } else {
            res &= 0b0111_1111;
        }

        self.f.c = right_bit;

        res
    }

    fn u8_rot_right(&mut self, val: u8) -> u8 {
        self.f.z = false;
        self.f.n = false;
        self.f.h = false;

        self.f.c = val & 1 == 1;

        val.rotate_right(1)
    }

    fn get_flag(&self, sym: &FlagSymbol) -> bool {
        match sym {
            FlagSymbol::Z => self.f.z,
            FlagSymbol::C => self.f.c,
        }
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

            0x80 => Instruction::ADD_A_r8(Reg8Symbol::B),
            0x81 => Instruction::ADD_A_r8(Reg8Symbol::C),
            0x82 => Instruction::ADD_A_r8(Reg8Symbol::D),
            0x83 => Instruction::ADD_A_r8(Reg8Symbol::E),
            0x84 => Instruction::ADD_A_r8(Reg8Symbol::H),
            0x85 => Instruction::ADD_A_r8(Reg8Symbol::L),
            0x86 => Instruction::ADD_A_HL,
            0x87 => Instruction::ADD_A_r8(Reg8Symbol::A),
            0x88 => Instruction::ADC_A_r8(Reg8Symbol::B),
            0x89 => Instruction::ADC_A_r8(Reg8Symbol::C),
            0x8A => Instruction::ADC_A_r8(Reg8Symbol::D),
            0x8B => Instruction::ADC_A_r8(Reg8Symbol::E),
            0x8C => Instruction::ADC_A_r8(Reg8Symbol::H),
            0x8D => Instruction::ADC_A_r8(Reg8Symbol::L),
            0x8E => Instruction::ADC_A_HL,
            0x8F => Instruction::ADC_A_r8(Reg8Symbol::A),

            0x90 => Instruction::SUB_A_r8(Reg8Symbol::B),
            0x91 => Instruction::SUB_A_r8(Reg8Symbol::C),
            0x92 => Instruction::SUB_A_r8(Reg8Symbol::D),
            0x93 => Instruction::SUB_A_r8(Reg8Symbol::E),
            0x94 => Instruction::SUB_A_r8(Reg8Symbol::H),
            0x95 => Instruction::SUB_A_r8(Reg8Symbol::L),
            0x96 => Instruction::SUB_A_HL,
            0x97 => Instruction::SUB_A_r8(Reg8Symbol::A),
            0x98 => Instruction::SBC_A_r8(Reg8Symbol::B),
            0x99 => Instruction::SBC_A_r8(Reg8Symbol::C),
            0x9A => Instruction::SBC_A_r8(Reg8Symbol::D),
            0x9B => Instruction::SBC_A_r8(Reg8Symbol::E),
            0x9C => Instruction::SBC_A_r8(Reg8Symbol::H),
            0x9D => Instruction::SBC_A_r8(Reg8Symbol::L),
            0x9E => Instruction::SBC_A_HL,
            0x9F => Instruction::SBC_A_r8(Reg8Symbol::A),

            0xA0 => Instruction::AND_A_r8(Reg8Symbol::B),
            0xA1 => Instruction::AND_A_r8(Reg8Symbol::C),
            0xA2 => Instruction::AND_A_r8(Reg8Symbol::D),
            0xA3 => Instruction::AND_A_r8(Reg8Symbol::E),
            0xA4 => Instruction::AND_A_r8(Reg8Symbol::H),
            0xA5 => Instruction::AND_A_r8(Reg8Symbol::L),
            0xA6 => Instruction::AND_A_HL,
            0xA7 => Instruction::AND_A_r8(Reg8Symbol::A),
            0xA8 => Instruction::XOR_A_r8(Reg8Symbol::B),
            0xA9 => Instruction::XOR_A_r8(Reg8Symbol::C),
            0xAA => Instruction::XOR_A_r8(Reg8Symbol::D),
            0xAB => Instruction::XOR_A_r8(Reg8Symbol::E),
            0xAC => Instruction::XOR_A_r8(Reg8Symbol::H),
            0xAD => Instruction::XOR_A_r8(Reg8Symbol::L),
            0xAE => Instruction::XOR_A_HL,
            0xAF => Instruction::XOR_A_r8(Reg8Symbol::A),

            0xB0 => Instruction::OR_A_r8(Reg8Symbol::B),
            0xB1 => Instruction::OR_A_r8(Reg8Symbol::C),
            0xB2 => Instruction::OR_A_r8(Reg8Symbol::D),
            0xB3 => Instruction::OR_A_r8(Reg8Symbol::E),
            0xB4 => Instruction::OR_A_r8(Reg8Symbol::H),
            0xB5 => Instruction::OR_A_r8(Reg8Symbol::L),
            0xB6 => Instruction::OR_A_HL,
            0xB7 => Instruction::OR_A_r8(Reg8Symbol::A),
            0xB8 => Instruction::CP_A_r8(Reg8Symbol::B),
            0xB9 => Instruction::CP_A_r8(Reg8Symbol::C),
            0xBA => Instruction::CP_A_r8(Reg8Symbol::D),
            0xBB => Instruction::CP_A_r8(Reg8Symbol::E),
            0xBC => Instruction::CP_A_r8(Reg8Symbol::H),
            0xBD => Instruction::CP_A_r8(Reg8Symbol::L),
            0xBE => Instruction::CP_A_HL,
            0xBF => Instruction::CP_A_r8(Reg8Symbol::A),

            0xC0 => Instruction::RET_Conditional(FlagSymbol::Z, false),
            0xC1 => Instruction::POP_r16(Reg16Symbol::BC),
            0xC2 => Instruction::JP_n16_Conditional(FlagSymbol::Z, false),
            0xC3 => Instruction::JP_n16,
            0xC4 => Instruction::CALL_n16_Conditional(FlagSymbol::Z, false),
            0xC5 => Instruction::PUSH_r16(Reg16Symbol::BC),
            0xC6 => Instruction::ADD_A_n8,
            0xC7 => Instruction::RST(0x00),
            0xC8 => Instruction::RET_Conditional(FlagSymbol::Z, true),
            0xC9 => Instruction::RET,
            0xCA => Instruction::JP_n16_Conditional(FlagSymbol::Z, true),
            0xCB => Instruction::PREFIX,
            0xCC => Instruction::CALL_n16_Conditional(FlagSymbol::Z, true),
            0xCD => Instruction::CALL_n16,
            0xCE => Instruction::ADC_A_n8,
            0xCF => Instruction::RST(0x08),

            0xD0 => Instruction::RET_Conditional(FlagSymbol::C, false),
            0xD1 => Instruction::POP_r16(Reg16Symbol::DE),
            0xD2 => Instruction::JP_n16_Conditional(FlagSymbol::C, false),
            0xD4 => Instruction::CALL_n16_Conditional(FlagSymbol::C, false),
            0xD5 => Instruction::PUSH_r16(Reg16Symbol::DE),
            0xD6 => Instruction::SUB_A_n8,
            0xD7 => Instruction::RST(0x10),
            0xD8 => Instruction::RET_Conditional(FlagSymbol::C, true),
            0xD9 => Instruction::RETI,
            0xDA => Instruction::JP_n16_Conditional(FlagSymbol::C, true),
            0xDC => Instruction::CALL_n16_Conditional(FlagSymbol::C, true),
            0xDE => Instruction::SBC_A_n8,
            0xDF => Instruction::RST(0x18),

            0xE0 => Instruction::LDH_n8_A,
            0xE1 => Instruction::POP_r16(Reg16Symbol::HL),
            0xE2 => Instruction::LDH_C_A,
            0xE5 => Instruction::PUSH_r16(Reg16Symbol::HL),
            0xE6 => Instruction::AND_A_n8,
            0xE7 => Instruction::RST(0x20),
            0xE8 => Instruction::ADD_SP_e8,
            0xE9 => Instruction::JP_HL,
            0xEA => Instruction::LD_a16_A,
            0xEE => Instruction::XOR_A_n8,
            0xEF => Instruction::RST(0x28),

            0xF0 => Instruction::LDH_A_n8,
            0xF1 => Instruction::POP_AF,
            0xF2 => Instruction::LDH_A_C,
            0xF3 => Instruction::DI,
            0xF5 => Instruction::PUSH_AF,
            0xF6 => Instruction::OR_A_n8,
            0xF7 => Instruction::RST(0x30),
            0xF8 => Instruction::LD_HL_SP_e8,
            0xF9 => Instruction::LD_SP_HL,
            0xFA => Instruction::LD_A_a16,
            0xFB => Instruction::EI,
            0xFE => Instruction::CP_A_n8,
            0xFF => Instruction::RST(0x38),

            _ => {
                println!("Warning: no implementation for opcode {:X}", opcode);
                Instruction::NotImplemented
            }
        }
    }

    fn decode_prefix(&self, opcode: u8) -> Instruction {
        match opcode {
            0x00 => Instruction::RLC(Reg8Symbol::B),
            0x01 => Instruction::RLC(Reg8Symbol::C),
            0x02 => Instruction::RLC(Reg8Symbol::D),
            0x03 => Instruction::RLC(Reg8Symbol::E),
            0x04 => Instruction::RLC(Reg8Symbol::H),
            0x05 => Instruction::RLC(Reg8Symbol::L),
            0x06 => Instruction::RLC_HL,
            0x07 => Instruction::RLC(Reg8Symbol::A),

            0x08 => Instruction::RRC(Reg8Symbol::B),
            0x09 => Instruction::RRC(Reg8Symbol::C),
            0x0A => Instruction::RRC(Reg8Symbol::D),
            0x0B => Instruction::RRC(Reg8Symbol::E),
            0x0C => Instruction::RRC(Reg8Symbol::H),
            0x0D => Instruction::RRC(Reg8Symbol::L),
            0x0E => Instruction::RRC_HL,
            0x0F => Instruction::RRC(Reg8Symbol::A),

            0x10 => Instruction::RL(Reg8Symbol::B),
            0x11 => Instruction::RL(Reg8Symbol::C),
            0x12 => Instruction::RL(Reg8Symbol::D),
            0x13 => Instruction::RL(Reg8Symbol::E),
            0x14 => Instruction::RL(Reg8Symbol::H),
            0x15 => Instruction::RL(Reg8Symbol::L),
            0x16 => Instruction::RL_HL,
            0x17 => Instruction::RL(Reg8Symbol::A),

            0x18 => Instruction::RR(Reg8Symbol::B),
            0x19 => Instruction::RR(Reg8Symbol::C),
            0x1A => Instruction::RR(Reg8Symbol::D),
            0x1B => Instruction::RR(Reg8Symbol::E),
            0x1C => Instruction::RR(Reg8Symbol::H),
            0x1D => Instruction::RR(Reg8Symbol::L),
            0x1E => Instruction::RR_HL,
            0x1F => Instruction::RR(Reg8Symbol::A),

            0x20 => Instruction::SLA(Reg8Symbol::B),
            0x21 => Instruction::SLA(Reg8Symbol::C),
            0x22 => Instruction::SLA(Reg8Symbol::D),
            0x23 => Instruction::SLA(Reg8Symbol::E),
            0x24 => Instruction::SLA(Reg8Symbol::H),
            0x25 => Instruction::SLA(Reg8Symbol::L),
            0x26 => Instruction::SLA_HL,
            0x27 => Instruction::SLA(Reg8Symbol::A),

            0x28 => Instruction::SRA(Reg8Symbol::B),
            0x29 => Instruction::SRA(Reg8Symbol::C),
            0x2A => Instruction::SRA(Reg8Symbol::D),
            0x2B => Instruction::SRA(Reg8Symbol::E),
            0x2C => Instruction::SRA(Reg8Symbol::H),
            0x2D => Instruction::SRA(Reg8Symbol::L),
            0x2E => Instruction::SRA_HL,
            0x2F => Instruction::SRA(Reg8Symbol::A),

            0x30 => Instruction::SWAP(Reg8Symbol::B),
            0x31 => Instruction::SWAP(Reg8Symbol::C),
            0x32 => Instruction::SWAP(Reg8Symbol::D),
            0x33 => Instruction::SWAP(Reg8Symbol::E),
            0x34 => Instruction::SWAP(Reg8Symbol::H),
            0x35 => Instruction::SWAP(Reg8Symbol::L),
            0x36 => Instruction::SWAP_HL,
            0x37 => Instruction::SWAP(Reg8Symbol::A),

            0x38 => Instruction::SRL(Reg8Symbol::B),
            0x39 => Instruction::SRL(Reg8Symbol::C),
            0x3A => Instruction::SRL(Reg8Symbol::D),
            0x3B => Instruction::SRL(Reg8Symbol::E),
            0x3C => Instruction::SRL(Reg8Symbol::H),
            0x3D => Instruction::SRL(Reg8Symbol::L),
            0x3E => Instruction::SRL_HL,
            0x3F => Instruction::SRL(Reg8Symbol::A),

            0x40 => Instruction::BIT(0, Reg8Symbol::B),
            0x41 => Instruction::BIT(0, Reg8Symbol::C),
            0x42 => Instruction::BIT(0, Reg8Symbol::D),
            0x43 => Instruction::BIT(0, Reg8Symbol::E),
            0x44 => Instruction::BIT(0, Reg8Symbol::H),
            0x45 => Instruction::BIT(0, Reg8Symbol::L),
            0x46 => Instruction::BIT_HL(0),
            0x47 => Instruction::BIT(0, Reg8Symbol::A),

            0x48 => Instruction::BIT(1, Reg8Symbol::B),
            0x49 => Instruction::BIT(1, Reg8Symbol::C),
            0x4A => Instruction::BIT(1, Reg8Symbol::D),
            0x4B => Instruction::BIT(1, Reg8Symbol::E),
            0x4C => Instruction::BIT(1, Reg8Symbol::H),
            0x4D => Instruction::BIT(1, Reg8Symbol::L),
            0x4E => Instruction::BIT_HL(1),
            0x4F => Instruction::BIT(1, Reg8Symbol::A),

            0x50 => Instruction::BIT(2, Reg8Symbol::B),
            0x51 => Instruction::BIT(2, Reg8Symbol::C),
            0x52 => Instruction::BIT(2, Reg8Symbol::D),
            0x53 => Instruction::BIT(2, Reg8Symbol::E),
            0x54 => Instruction::BIT(2, Reg8Symbol::H),
            0x55 => Instruction::BIT(2, Reg8Symbol::L),
            0x56 => Instruction::BIT_HL(2),
            0x57 => Instruction::BIT(2, Reg8Symbol::A),

            0x58 => Instruction::BIT(3, Reg8Symbol::B),
            0x59 => Instruction::BIT(3, Reg8Symbol::C),
            0x5A => Instruction::BIT(3, Reg8Symbol::D),
            0x5B => Instruction::BIT(3, Reg8Symbol::E),
            0x5C => Instruction::BIT(3, Reg8Symbol::H),
            0x5D => Instruction::BIT(3, Reg8Symbol::L),
            0x5E => Instruction::BIT_HL(3),
            0x5F => Instruction::BIT(3, Reg8Symbol::A),

            0x60 => Instruction::BIT(4, Reg8Symbol::B),
            0x61 => Instruction::BIT(4, Reg8Symbol::C),
            0x62 => Instruction::BIT(4, Reg8Symbol::D),
            0x63 => Instruction::BIT(4, Reg8Symbol::E),
            0x64 => Instruction::BIT(4, Reg8Symbol::H),
            0x65 => Instruction::BIT(4, Reg8Symbol::L),
            0x66 => Instruction::BIT_HL(4),
            0x67 => Instruction::BIT(4, Reg8Symbol::A),

            0x68 => Instruction::BIT(5, Reg8Symbol::B),
            0x69 => Instruction::BIT(5, Reg8Symbol::C),
            0x6A => Instruction::BIT(5, Reg8Symbol::D),
            0x6B => Instruction::BIT(5, Reg8Symbol::E),
            0x6C => Instruction::BIT(5, Reg8Symbol::H),
            0x6D => Instruction::BIT(5, Reg8Symbol::L),
            0x6E => Instruction::BIT_HL(5),
            0x6F => Instruction::BIT(5, Reg8Symbol::A),

            0x70 => Instruction::BIT(6, Reg8Symbol::B),
            0x71 => Instruction::BIT(6, Reg8Symbol::C),
            0x72 => Instruction::BIT(6, Reg8Symbol::D),
            0x73 => Instruction::BIT(6, Reg8Symbol::E),
            0x74 => Instruction::BIT(6, Reg8Symbol::H),
            0x75 => Instruction::BIT(6, Reg8Symbol::L),
            0x76 => Instruction::BIT_HL(6),
            0x77 => Instruction::BIT(6, Reg8Symbol::A),

            0x78 => Instruction::BIT(7, Reg8Symbol::B),
            0x79 => Instruction::BIT(7, Reg8Symbol::C),
            0x7A => Instruction::BIT(7, Reg8Symbol::D),
            0x7B => Instruction::BIT(7, Reg8Symbol::E),
            0x7C => Instruction::BIT(7, Reg8Symbol::H),
            0x7D => Instruction::BIT(7, Reg8Symbol::L),
            0x7E => Instruction::BIT_HL(7),
            0x7F => Instruction::BIT(7, Reg8Symbol::A),

            0x80 => Instruction::RES(0, Reg8Symbol::B),
            0x81 => Instruction::RES(0, Reg8Symbol::C),
            0x82 => Instruction::RES(0, Reg8Symbol::D),
            0x83 => Instruction::RES(0, Reg8Symbol::E),
            0x84 => Instruction::RES(0, Reg8Symbol::H),
            0x85 => Instruction::RES(0, Reg8Symbol::L),
            0x86 => Instruction::RES_HL(0),
            0x87 => Instruction::RES(0, Reg8Symbol::A),

            0x88 => Instruction::RES(1, Reg8Symbol::B),
            0x89 => Instruction::RES(1, Reg8Symbol::C),
            0x8A => Instruction::RES(1, Reg8Symbol::D),
            0x8B => Instruction::RES(1, Reg8Symbol::E),
            0x8C => Instruction::RES(1, Reg8Symbol::H),
            0x8D => Instruction::RES(1, Reg8Symbol::L),
            0x8E => Instruction::RES_HL(1),
            0x8F => Instruction::RES(1, Reg8Symbol::A),

            0x90 => Instruction::RES(2, Reg8Symbol::B),
            0x91 => Instruction::RES(2, Reg8Symbol::C),
            0x92 => Instruction::RES(2, Reg8Symbol::D),
            0x93 => Instruction::RES(2, Reg8Symbol::E),
            0x94 => Instruction::RES(2, Reg8Symbol::H),
            0x95 => Instruction::RES(2, Reg8Symbol::L),
            0x96 => Instruction::RES_HL(2),
            0x97 => Instruction::RES(2, Reg8Symbol::A),

            0x98 => Instruction::RES(3, Reg8Symbol::B),
            0x99 => Instruction::RES(3, Reg8Symbol::C),
            0x9A => Instruction::RES(3, Reg8Symbol::D),
            0x9B => Instruction::RES(3, Reg8Symbol::E),
            0x9C => Instruction::RES(3, Reg8Symbol::H),
            0x9D => Instruction::RES(3, Reg8Symbol::L),
            0x9E => Instruction::RES_HL(3),
            0x9F => Instruction::RES(3, Reg8Symbol::A),

            0xA0 => Instruction::RES(4, Reg8Symbol::B),
            0xA1 => Instruction::RES(4, Reg8Symbol::C),
            0xA2 => Instruction::RES(4, Reg8Symbol::D),
            0xA3 => Instruction::RES(4, Reg8Symbol::E),
            0xA4 => Instruction::RES(4, Reg8Symbol::H),
            0xA5 => Instruction::RES(4, Reg8Symbol::L),
            0xA6 => Instruction::RES_HL(4),
            0xA7 => Instruction::RES(4, Reg8Symbol::A),

            0xA8 => Instruction::RES(5, Reg8Symbol::B),
            0xA9 => Instruction::RES(5, Reg8Symbol::C),
            0xAA => Instruction::RES(5, Reg8Symbol::D),
            0xAB => Instruction::RES(5, Reg8Symbol::E),
            0xAC => Instruction::RES(5, Reg8Symbol::H),
            0xAD => Instruction::RES(5, Reg8Symbol::L),
            0xAE => Instruction::RES_HL(5),
            0xAF => Instruction::RES(5, Reg8Symbol::A),

            0xB0 => Instruction::RES(6, Reg8Symbol::B),
            0xB1 => Instruction::RES(6, Reg8Symbol::C),
            0xB2 => Instruction::RES(6, Reg8Symbol::D),
            0xB3 => Instruction::RES(6, Reg8Symbol::E),
            0xB4 => Instruction::RES(6, Reg8Symbol::H),
            0xB5 => Instruction::RES(6, Reg8Symbol::L),
            0xB6 => Instruction::RES_HL(6),
            0xB7 => Instruction::RES(6, Reg8Symbol::A),

            0xB8 => Instruction::RES(7, Reg8Symbol::B),
            0xB9 => Instruction::RES(7, Reg8Symbol::C),
            0xBA => Instruction::RES(7, Reg8Symbol::D),
            0xBB => Instruction::RES(7, Reg8Symbol::E),
            0xBC => Instruction::RES(7, Reg8Symbol::H),
            0xBD => Instruction::RES(7, Reg8Symbol::L),
            0xBE => Instruction::RES_HL(7),
            0xBF => Instruction::RES(7, Reg8Symbol::A),

            0xC0 => Instruction::SET(0, Reg8Symbol::B),
            0xC1 => Instruction::SET(0, Reg8Symbol::C),
            0xC2 => Instruction::SET(0, Reg8Symbol::D),
            0xC3 => Instruction::SET(0, Reg8Symbol::E),
            0xC4 => Instruction::SET(0, Reg8Symbol::H),
            0xC5 => Instruction::SET(0, Reg8Symbol::L),
            0xC6 => Instruction::SET_HL(0),
            0xC7 => Instruction::SET(0, Reg8Symbol::A),

            0xC8 => Instruction::SET(1, Reg8Symbol::B),
            0xC9 => Instruction::SET(1, Reg8Symbol::C),
            0xCA => Instruction::SET(1, Reg8Symbol::D),
            0xCB => Instruction::SET(1, Reg8Symbol::E),
            0xCC => Instruction::SET(1, Reg8Symbol::H),
            0xCD => Instruction::SET(1, Reg8Symbol::L),
            0xCE => Instruction::SET_HL(1),
            0xCF => Instruction::SET(1, Reg8Symbol::A),

            0xD0 => Instruction::SET(2, Reg8Symbol::B),
            0xD1 => Instruction::SET(2, Reg8Symbol::C),
            0xD2 => Instruction::SET(2, Reg8Symbol::D),
            0xD3 => Instruction::SET(2, Reg8Symbol::E),
            0xD4 => Instruction::SET(2, Reg8Symbol::H),
            0xD5 => Instruction::SET(2, Reg8Symbol::L),
            0xD6 => Instruction::SET_HL(2),
            0xD7 => Instruction::SET(2, Reg8Symbol::A),

            0xD8 => Instruction::SET(3, Reg8Symbol::B),
            0xD9 => Instruction::SET(3, Reg8Symbol::C),
            0xDA => Instruction::SET(3, Reg8Symbol::D),
            0xDB => Instruction::SET(3, Reg8Symbol::E),
            0xDC => Instruction::SET(3, Reg8Symbol::H),
            0xDD => Instruction::SET(3, Reg8Symbol::L),
            0xDE => Instruction::SET_HL(3),
            0xDF => Instruction::SET(3, Reg8Symbol::A),

            0xE0 => Instruction::SET(4, Reg8Symbol::B),
            0xE1 => Instruction::SET(4, Reg8Symbol::C),
            0xE2 => Instruction::SET(4, Reg8Symbol::D),
            0xE3 => Instruction::SET(4, Reg8Symbol::E),
            0xE4 => Instruction::SET(4, Reg8Symbol::H),
            0xE5 => Instruction::SET(4, Reg8Symbol::L),
            0xE6 => Instruction::SET_HL(4),
            0xE7 => Instruction::SET(4, Reg8Symbol::A),

            0xE8 => Instruction::SET(5, Reg8Symbol::B),
            0xE9 => Instruction::SET(5, Reg8Symbol::C),
            0xEA => Instruction::SET(5, Reg8Symbol::D),
            0xEB => Instruction::SET(5, Reg8Symbol::E),
            0xEC => Instruction::SET(5, Reg8Symbol::H),
            0xED => Instruction::SET(5, Reg8Symbol::L),
            0xEE => Instruction::SET_HL(5),
            0xEF => Instruction::SET(5, Reg8Symbol::A),

            0xF0 => Instruction::SET(6, Reg8Symbol::B),
            0xF1 => Instruction::SET(6, Reg8Symbol::C),
            0xF2 => Instruction::SET(6, Reg8Symbol::D),
            0xF3 => Instruction::SET(6, Reg8Symbol::E),
            0xF4 => Instruction::SET(6, Reg8Symbol::H),
            0xF5 => Instruction::SET(6, Reg8Symbol::L),
            0xF6 => Instruction::SET_HL(6),
            0xF7 => Instruction::SET(6, Reg8Symbol::A),

            0xF8 => Instruction::SET(7, Reg8Symbol::B),
            0xF9 => Instruction::SET(7, Reg8Symbol::C),
            0xFA => Instruction::SET(7, Reg8Symbol::D),
            0xFB => Instruction::SET(7, Reg8Symbol::E),
            0xFC => Instruction::SET(7, Reg8Symbol::H),
            0xFD => Instruction::SET(7, Reg8Symbol::L),
            0xFE => Instruction::SET_HL(7),
            0xFF => Instruction::SET(7, Reg8Symbol::A),
        }
    }

    // Instructions take a variable amount of CPU cycles
    // from https://emudev.de/gameboy-emulator/opcode-cycles-and-timings/ return the number of
    // cycles executed to develop accurate timing. Yields number of T-states.
    fn execute(&mut self, inst: Instruction) -> u8 {
        match inst {
            Instruction::NOP => 4,

            Instruction::LD_r8_r8(r8s1, r8s2) => {
                // copy value at r82 to r81
                let val = self.get_r8(&r8s2);
                self.set_r8(&r8s1, val);
                4
            }

            Instruction::LD_r8_n8(r8s) => {
                let val = self.fetch();
                self.set_r8(&r8s, val);
                8
            }

            Instruction::LD_r8_r16(r8s, r16s) => {
                // get value pointed to by r16 and store in r8
                let addr = self.get_r16(&r16s) as usize;
                let val = self.bus.read(addr as usize);
                self.set_r8(&r8s, val);

                if r16s == Reg16Symbol::HLI {
                    self.inc_r16(r16s);
                } else if r16s == Reg16Symbol::HLD {
                    self.dec_r16(r16s);
                }

                8
            }

            Instruction::LD_r16_r8(r16s, r8s) => {
                let address = self.get_r16(&r16s) as usize;
                let value = self.get_r8(&r8s);
                self.bus.write(address, value);

                if r16s == Reg16Symbol::HLI {
                    self.inc_r16(r16s);
                } else if r16s == Reg16Symbol::HLD {
                    self.dec_r16(r16s);
                }

                8
            }

            Instruction::LD_r16_n16(r16s) => {
                let val = self.fetch_2();
                self.set_r16(&r16s, val);
                12
            }

            Instruction::LD_a16_SP => {
                let addr = self.fetch_2();

                // write SP & 0xFF (low byte) to addr, and SP >> 8 (high byte) to addr + 1
                self.bus.write(addr as usize, (self.sp & 0xFF) as u8);
                self.bus.write((addr + 1) as usize, (self.sp >> 8) as u8);
                20
            }

            Instruction::LD_SP_n16 => {
                let addr = self.fetch_2();
                self.sp = addr;
                12
            }

            Instruction::LD_HL_n8 => {
                let value = self.fetch();
                let address = self.get_r16(&Reg16Symbol::HL) as usize;
                self.bus.write(address, value);
                12
            }

            Instruction::INC_r16(r16s) => self.inc_r16(r16s),

            Instruction::INC_r8(r8s) => {
                let r = self.get_r8(&r8s);

                self.f.n = false;

                // check overflow from bit 3 (bits 0-3 are on)
                self.f.h = r & 0xF == 0xF;

                let new_val = r.wrapping_add(1);
                self.f.z = new_val == 0;

                self.set_r8(&r8s, new_val);
                4
            }

            Instruction::INC_SP => {
                self.sp = self.sp.wrapping_add(1);
                8
            }

            Instruction::DEC_r16(r16s) => self.dec_r16(r16s),

            Instruction::DEC_r8(r8s) => {
                let r = self.get_r8(&r8s);

                self.f.n = true;

                // check if a borrow from bit 4 is required (bits 0-3 are off)
                if r & 0x0F == 0 {
                    self.f.h = true;
                }

                let new_val = r.wrapping_sub(1);
                if new_val == 0 {
                    self.f.z = true;
                }

                self.set_r8(&r8s, new_val);
                4
            }

            Instruction::DEC_SP => {
                self.sp = self.sp.wrapping_sub(1);
                8
            }

            Instruction::ADD_HL_r16(r16s) => {
                self.f.n = false;

                let hl = self.get_r16(&Reg16Symbol::HL);
                let r16_val = self.get_r16(&r16s);

                let res = self.sum_u16_with_flags(hl, r16_val);

                self.set_r16(&Reg16Symbol::HL, res);
                8
            }

            Instruction::ADD_HL_SP => {
                self.f.n = false;

                let hl = self.get_r16(&Reg16Symbol::HL);

                let res = self.sum_u16_with_flags(hl, self.sp);
                self.set_r16(&Reg16Symbol::HL, res);
                8
            }

            Instruction::ADD_A_r8(r8s) => {
                let val = self.get_r8(&r8s);

                let res = self.sum_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);
                self.set_r8(&Reg8Symbol::A, res);

                self.f.z = res == 0;
                self.f.n = false;
                4
            }

            Instruction::ADD_A_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);

                let res = self.sum_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);
                self.set_r8(&Reg8Symbol::A, res);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::ADD_A_n8 => {
                let val = self.fetch();
                let res = self.sum_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::ADD_SP_e8 => {
                let ofs = self.fetch();
                let ofs_signed = ofs as i8;
                let ofs_u16 = ofs_signed as u16;

                // set h and c flags as u8 addition
                let sp_low = (self.sp & 0xFF) as u8;
                self.f.h = (sp_low & 0x0F) + (ofs & 0x0F) > 0x0F;
                self.f.c = (sp_low as u16) + (ofs as u16) > 0xFF;

                self.sp = self.sp.wrapping_add(ofs_u16);

                self.f.z = self.sp == 0;
                self.f.n = false;
                16
            }

            Instruction::ADC_A_r8(r8s) => {
                let carry = if self.f.c { 1 } else { 0 };
                let reg = self.get_r8(&r8s);
                let val = reg.wrapping_add(carry);

                let res = self.sum_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                4
            }

            Instruction::ADC_A_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);

                let res = self.sum_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::ADC_A_n8 => {
                let carry = if self.f.c { 1 } else { 0 };
                let val = self.fetch();

                let res = self.sum_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::SUB_A_r8(r8s) => {
                let val = self.get_r8(&r8s);

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                4
            }

            Instruction::SUB_A_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::SUB_A_n8 => {
                let val = self.fetch();

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::SBC_A_r8(r8s) => {
                let carry = if self.f.c { 1 } else { 0 };
                let reg = self.get_r8(&r8s);
                let val = reg.wrapping_add(carry);

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                4
            }

            Instruction::SBC_A_HL => {
                let carry = if self.f.c { 1 } else { 0 };
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize).wrapping_add(carry);

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::SBC_A_n8 => {
                let carry = if self.f.c { 1 } else { 0 };
                let val = self.fetch();

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::AND_A_r8(r8s) => {
                let val = self.get_r8(&r8s);

                let res = self.get_r8(&Reg8Symbol::A) & val;
                self.set_r8(&Reg8Symbol::A, res);

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = true;
                self.f.c = false;
                4
            }

            Instruction::AND_A_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);

                let res = self.get_r8(&Reg8Symbol::A) & val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = true;
                self.f.c = false;
                8
            }

            Instruction::AND_A_n8 => {
                let val = self.fetch();

                let res = self.get_r8(&Reg8Symbol::A) & val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = true;
                self.f.c = false;
                8
            }

            Instruction::XOR_A_r8(r8s) => {
                let val = self.get_r8(&r8s);

                let res = self.get_r8(&Reg8Symbol::A) ^ val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                4
            }

            Instruction::XOR_A_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);

                let res = self.get_r8(&Reg8Symbol::A) ^ val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                8
            }

            Instruction::XOR_A_n8 => {
                let val = self.fetch();

                let res = self.get_r8(&Reg8Symbol::A) ^ val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                8
            }

            Instruction::OR_A_r8(r8s) => {
                let val = self.get_r8(&r8s);

                let res = self.get_r8(&Reg8Symbol::A) | val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                4
            }

            Instruction::OR_A_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);

                let res = self.get_r8(&Reg8Symbol::A) | val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                8
            }

            Instruction::OR_A_n8 => {
                let val = self.fetch();

                let res = self.get_r8(&Reg8Symbol::A) | val;

                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                8
            }

            Instruction::CP_A_r8(r8s) => {
                let val = self.get_r8(&r8s);

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                4
            }

            Instruction::CP_A_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::CP_A_n8 => {
                let val = self.fetch();

                let res = self.sub_u8_with_flags(self.get_r8(&Reg8Symbol::A), val);

                self.f.z = res == 0;
                self.f.n = false;
                8
            }

            Instruction::JP_n16 => {
                let addr = self.fetch_2();
                self.pc = addr;
                16
            }

            Instruction::JP_n16_Conditional(fls, on) => {
                let flag = self.get_flag(&fls);
                if flag == on {
                    let addr = self.fetch_2();
                    self.pc = addr;
                    16
                } else {
                    12
                }
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

            Instruction::RET => {
                let val = self.stack_pop_u16();
                self.pc = val;
                16
            }

            Instruction::RET_Conditional(fls, on) => {
                let flag = self.get_flag(&fls);
                if flag != on {
                    return 8;
                }

                let val = self.stack_pop_u16();
                self.pc = val;
                20
            }

            Instruction::PUSH_r16(r16s) => {
                let val = self.get_r16(&r16s);
                self.stack_push_u16(val);
                16
            }

            Instruction::POP_r16(r16s) => {
                let val = self.stack_pop_u16();
                self.set_r16(&r16s, val);
                12
            }

            Instruction::POP_AF => {
                let f_new = self.stack_pop_u8() & 0xF0;
                self.f.set_from_u8(f_new >> 4);

                let a_new = self.stack_pop_u8();
                self.set_r8(&Reg8Symbol::A, a_new);
                12
            }

            Instruction::CALL_n16 => {
                let ret_addr = self.pc + 3;
                self.stack_push_u16(ret_addr);

                let jump_addr = self.fetch_2();
                self.pc = jump_addr;
                24
            }

            Instruction::CALL_n16_Conditional(fls, on) => {
                let flag = self.get_flag(&fls);
                if flag != on {
                    return 12;
                }

                let ret_addr = self.pc + 3;
                self.stack_push_u16(ret_addr);

                let jump_addr = self.fetch_2();
                self.pc = jump_addr;
                24
            }

            Instruction::RST(addr) => {
                let ret_addr = self.pc;
                self.stack_push_u16(ret_addr);
                self.pc = addr;
                16
            }

            Instruction::RLCA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_left(val);
                self.set_r8(&Reg8Symbol::A, val);
                4
            }

            Instruction::RRCA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_right(val);
                self.set_r8(&Reg8Symbol::A, val);
                4
            }

            Instruction::RLA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_left_through_carry(val);
                self.set_r8(&Reg8Symbol::A, val);
                4
            }

            Instruction::RRA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_right_through_carry(val);
                self.set_r8(&Reg8Symbol::A, val);
                4
            }

            Instruction::DAA => {
                let mut adj = 0;
                self.f.h = false;

                if self.f.n {
                    if self.f.h {
                        adj += 0x06;
                    }

                    if self.f.c {
                        adj += 0x60;
                    }

                    self.set_r8(&Reg8Symbol::A, self.get_r8(&Reg8Symbol::A) - adj);
                } else {
                    if self.f.h || self.get_r8(&Reg8Symbol::A) & 0x0F > 0x09 {
                        adj += 0x06;
                    }

                    if self.f.c || self.get_r8(&Reg8Symbol::A) > 0x99 {
                        adj += 0x60;
                        self.f.c = true;
                    }

                    self.set_r8(&Reg8Symbol::A, self.get_r8(&Reg8Symbol::A) + adj);
                }

                if self.get_r8(&Reg8Symbol::A) == 0 {
                    self.f.z = true;
                }

                4
            }

            Instruction::CPL => {
                self.f.n = true;
                self.f.h = true;

                self.set_r8(&Reg8Symbol::A, !self.get_r8(&Reg8Symbol::A));
                4
            }

            Instruction::SCF => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = true;
                4
            }

            Instruction::CCF => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = !self.f.c;
                4
            }

            Instruction::STOP => {
                todo!();
            }

            Instruction::HALT => {
                todo!();
            }

            Instruction::PREFIX => {
                let op = self.fetch();
                let inst = self.decode_prefix(op);
                4 + self.execute(inst)
            }

            Instruction::RLC(r8s) => {
                let val = self.get_r8(&r8s);
                let new = self.u8_rot_left(val);
                self.set_r8(&r8s, new);
                self.f.z = new == 0;
                8
            }

            Instruction::RLC_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                let new = self.u8_rot_left(val);
                self.bus.write(hl as usize, new);
                self.f.z = new == 0;
                12
            }

            Instruction::RL(r8s) => {
                let val = self.get_r8(&r8s);
                let new = self.u8_rot_left_through_carry(val);
                self.set_r8(&r8s, new);
                self.f.z = new == 0;
                8
            }

            Instruction::RL_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                let new = self.u8_rot_left_through_carry(val);
                self.bus.write(hl as usize, new);
                self.f.z = new == 0;
                12
            }

            Instruction::RRC(r8s) => {
                let val = self.get_r8(&r8s);
                let new = self.u8_rot_right(val);
                self.set_r8(&r8s, new);
                self.f.z = new == 0;
                8
            }

            Instruction::RRC_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                let new = self.u8_rot_right(val);
                self.bus.write(hl as usize, new);
                self.f.z = new == 0;
                12
            }

            Instruction::RR(r8s) => {
                let val = self.get_r8(&r8s);
                let new = self.u8_rot_right_through_carry(val);
                self.set_r8(&r8s, new);
                self.f.z = new == 0;
                8
            }

            Instruction::RR_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                let new = self.u8_rot_right_through_carry(val);
                self.bus.write(hl as usize, new);
                self.f.z = new == 0;
                12
            }

            Instruction::SLA(r8s) => {
                let val = self.get_r8(&r8s);
                self.f.c = val & 0b1000_0000 == 1;
                let new = val << 1;
                self.f.z = new == 0;
                self.f.n = false;
                self.f.h = false;
                self.set_r8(&r8s, new);
                8
            }

            Instruction::SLA_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                self.f.c = val & 0b1000_0000 == 1;
                let new = val << 1;
                self.bus.write(hl as usize, new);
                self.f.z = new == 0;
                self.f.n = false;
                self.f.h = false;
                12
            }

            Instruction::SRA(r8s) => {
                let val = self.get_r8(&r8s);
                self.f.c = val & 1 == 1;
                let new = val >> 1;
                self.f.z = new == 0;
                self.f.n = false;
                self.f.h = false;
                self.set_r8(&r8s, new);
                8
            }

            Instruction::SRA_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                self.f.c = val & 1 == 1;
                let new = val >> 1;
                self.bus.write(hl as usize, new);
                self.f.z = new == 0;
                self.f.n = false;
                self.f.h = false;
                12
            }

            Instruction::SWAP(r8s) => {
                let val = self.get_r8(&r8s);
                let high = val & 0xF0;
                let low = val & 0x0F;
                let new = (low << 4) | high;
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                self.set_r8(&r8s, new);
                8
            }

            Instruction::SWAP_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                let high = val & 0xF0;
                let low = val & 0x0F;
                let new = (low << 4) | high;
                self.bus.write(hl as usize, new);
                16
            }

            Instruction::SRL(r8s) => {
                let val = self.get_r8(&r8s);
                self.f.c = val & 1 == 1;
                let new = val >> 1;
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = false;
                self.set_r8(&r8s, new);
                8
            }

            Instruction::SRL_HL => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize);
                self.f.c = val & 1 == 1;
                let new = val >> 1;
                self.bus.write(hl as usize, new);
                self.f.z = new == 0;
                self.f.n = false;
                self.f.h = false;
                12
            }

            Instruction::BIT(n, r8s) => {
                let reg = self.get_r8(&r8s);
                let val = reg >> n;
                self.f.z = val & 1 == 0;
                self.f.n = false;
                self.f.h = true;
                8
            }

            Instruction::BIT_HL(n) => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize) >> n;
                self.f.c = val & 1 == 0;
                self.f.n = false;
                self.f.h = true;
                12
            }

            Instruction::RES(n, r8s) => {
                let reg = self.get_r8(&r8s);
                self.set_r8(&r8s, reg & !(1 << n));
                8
            }

            Instruction::RES_HL(n) => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize) & !(1 << n);
                self.bus.write(hl as usize, val);
                12
            }

            Instruction::SET(n, r8s) => {
                let reg = self.get_r8(&r8s);
                self.set_r8(&r8s, reg | (1 << n));
                8
            }

            Instruction::SET_HL(n) => {
                let hl = self.get_r16(&Reg16Symbol::HL);
                let val = self.bus.read(hl as usize) | (1 << n);
                self.bus.write(hl as usize, val);
                12
            }

            Instruction::NotImplemented => 4,

            _ => todo!(), // RETI, DI, EI
        }
    }

    pub fn step(&mut self) {
        let opcode = self.fetch();
        let inst = self.decode(opcode);
        self.execute(inst);
    }
}
