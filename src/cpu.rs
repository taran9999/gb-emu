use std::cell::Cell;

use crate::bus::Bus;
use crate::instruction::{FlagSymbol, Instruction, Reg16Symbol, Reg8Symbol};

// #[cfg(test)]
// mod tests;

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
    ime: bool,
    set_ime: bool,
    halted: bool,
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
            ime: false,
            set_ime: false,
            halted: false,
            bus,
        }
    }

    pub fn export_state(&self) -> String {
        format!("A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X}",
        self.a.get(), self.f.to_u8(), self.b.get(), self.c.get(),
        self.d.get(), self.e.get(), self.h.get(), self.l.get(),
        self.sp, self.pc)
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

            Instruction::LD_SP_HL => {
                let val = self.get_r16(&Reg16Symbol::HL);
                self.sp = val;
                8
            }

            Instruction::LD_a16_A => {
                let val = self.get_r8(&Reg8Symbol::A);
                let addr = self.fetch_2();
                self.bus.write(addr as usize, val);
                16
            }

            Instruction::LD_A_a16 => {
                let addr = self.fetch_2() as usize;
                let val = self.bus.read(addr);
                self.set_r8(&Reg8Symbol::A, val);
                16
            }

            Instruction::LD_HL_n8 => {
                let value = self.fetch();
                let address = self.get_r16(&Reg16Symbol::HL) as usize;
                self.bus.write(address, value);
                12
            }

            // NOTE: should this actually add to sp or no
            Instruction::LD_HL_SP_e8 => {
                self.execute(Instruction::ADD_SP_e8);
                self.set_r16(&Reg16Symbol::HL, self.sp);
                12
            }

            Instruction::LDH_n8_A => {
                let ofs = self.fetch();
                let addr = ofs as u16 + 0xFF00;
                self.bus.write(addr as usize, self.get_r8(&Reg8Symbol::A));
                12
            }

            Instruction::LDH_A_n8 => {
                let ofs = self.fetch();
                let addr = ofs as u16 + 0xFF00;
                let val = self.bus.read(addr as usize);
                self.set_r8(&Reg8Symbol::A, val);
                12
            }

            Instruction::LDH_C_A => {
                let mut addr: usize = 0xFF00;
                if self.f.c {
                    addr += 1
                }
                let val = self.get_r8(&Reg8Symbol::A);
                self.bus.write(addr, val);
                8
            }

            Instruction::LDH_A_C => {
                let mut addr: usize = 0xFF00;
                if self.f.c {
                    addr += 1;
                }
                let val = self.bus.read(addr);
                self.set_r8(&Reg8Symbol::A, val);
                8
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
                let val = self.fetch() + carry;

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
                let val = self.fetch() + carry;

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

            Instruction::JP_HL => {
                let val = self.get_r16(&Reg16Symbol::HL);
                self.pc = val;
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

            Instruction::RETI => todo!(),

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

            Instruction::PUSH_AF => {
                let a = self.get_r8(&Reg8Symbol::A);
                self.stack_push_u8(a);

                let f = self.f.to_u8();
                self.stack_push_u8(f << 4);
                16
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

            Instruction::DI => {
                self.set_ime = false;
                self.ime = false;
                4
            }

            Instruction::EI => {
                self.set_ime = true;
                4
            }

            Instruction::STOP => {
                todo!();
            }

            Instruction::HALT => {
                self.halted = true;
                4
            }

            Instruction::PREFIX => {
                let op = self.fetch();
                let inst = Instruction::decode_prefix(op);
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
        }
    }

    pub fn step(&mut self) {
        if self.halted {
            return;
        }

        if self.set_ime {
            self.set_ime = false;
            self.ime = true;
        }

        let opcode = self.fetch();
        let inst = Instruction::decode(opcode);
        self.execute(inst);
        self.handle_interrupt();
    }

    fn handle_interrupt(&mut self) -> u8 {
        if !self.ime {
            return 0;
        }

        20
    }
}
