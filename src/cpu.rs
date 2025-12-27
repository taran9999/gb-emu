use std::cell::{Cell, RefCell};
use std::io::Write;
use std::{fmt, fs};
use std::path::Path;

use crate::bus::Bus;
use crate::instruction::{Condition, Instruction, Op16, Op8, Reg16Symbol, Reg8Symbol};
use crate::interrupts::Interrupt;

// #[cfg(test)]
// mod tests;

#[derive(Clone)]
struct Flags {
    z: bool,
    n: bool,
    h: bool,
    c: bool,
}

impl Flags {
    fn new() -> Flags {
        Flags {
            z: true,
            n: false,
            h: true,
            c: true,
        }
    }

    fn to_u8(&self) -> u8 {
        let mut res: u8 = 0;
        if self.z {
            res |= 1u8 << 7;
        }

        if self.n {
            res |= 1u8 << 6;
        }

        if self.h {
            res |= 1u8 << 5;
        }

        if self.c {
            res |= 1u8 << 4;
        }

        res
    }

    fn set_from_u8(&mut self, val: u8) {
        self.z = (val >> 7) & 1 == 1;
        self.n = (val >> 6) & 1 == 1;
        self.h = (val >> 5) & 1 == 1;
        self.c = (val >> 4) & 1 == 1;
    }
}

impl fmt::Display for Flags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}{} ({:02X})",
            if self.z { "Z" } else { "-" },
            if self.n { "N" } else { "-" },
            if self.h { "H" } else { "-" },
            if self.c { "C" } else { "-" },
            self.to_u8()
        )
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
    bus: &'a RefCell<Bus<'a>>,
}

/*
* Timing: add M-cycles for the following actions
* Fetch byte: 1
* Stack push u8: 2 (data bus waits for IDU to decrement SP first)
* Stack push u16: 3 (the second SP decrement is done concurrently with the first bus write)
* Stack pop u8: 1
* Set PC from bytes/signed offset: 1 (can only set on the cycle after both bytes are fetched)
* Set PC from HL: 0
* Bus read/write: 1
*/
impl CPU<'_> {
    pub fn init<'a>(bus: &'a RefCell<Bus<'a>>) -> CPU<'a> {
        CPU {
            a: Cell::new(0x01),
            b: Cell::new(0),
            c: Cell::new(0x13),
            d: Cell::new(0),
            e: Cell::new(0xD8),
            h: Cell::new(0x01),
            l: Cell::new(0x4D),
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
        format!("A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}\n",
        self.a.get(),
        self.f.to_u8(),
        self.b.get(), self.c.get(), self.d.get(),
        self.e.get(), self.h.get(), self.l.get(),
        self.sp, self.pc,
        self.bus_read(self.pc as usize).0, 
        self.bus_read((self.pc + 1) as usize).0, 
        self.bus_read((self.pc + 2) as usize).0, 
        self.bus_read((self.pc + 3) as usize).0)
    }

    fn bus_read(&self, addr: usize) -> (u8, u8) {
        (self.bus.borrow().read(addr), 1)
    }

    fn bus_write(&mut self, addr: usize, val: u8) -> u8 {
        self.bus.borrow_mut().write(addr, val);
        1
    }

    fn fetch(&mut self) -> (u8, u8) {
        let (byte, _) = self.bus_read(self.pc as usize);
        self.pc += 1;
        (byte, 1)
    }

    // fetch two bytes and return a u16 (little endian)
    fn fetch_2(&mut self) -> (u16, u8) {
        let low = self.fetch().0 as u16;
        let high = self.fetch().0 as u16;
        ((high << 8) | low, 2)
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

    fn get_r16(&mut self, sym: &Reg16Symbol) -> u16 {
        if sym == &Reg16Symbol::SP {
            return self.sp;
        }

        let (high, low) = match sym {
            Reg16Symbol::BC => (self.get_r8(&Reg8Symbol::B), self.get_r8(&Reg8Symbol::C)),
            Reg16Symbol::DE => (self.get_r8(&Reg8Symbol::D), self.get_r8(&Reg8Symbol::E)),
            Reg16Symbol::HL => (self.get_r8(&Reg8Symbol::H), self.get_r8(&Reg8Symbol::L)),

            // don't count additional m cycle for inc/dec as IDU does it concurrently with mem read
            Reg16Symbol::HLI => {
                let (h, l) = (self.get_r8(&Reg8Symbol::H), self.get_r8(&Reg8Symbol::L));
                self.inc_r16(Reg16Symbol::HL);
                (h, l)
            }

            Reg16Symbol::HLD => {
                let (h, l) = (self.get_r8(&Reg8Symbol::H), self.get_r8(&Reg8Symbol::L));
                self.dec_r16(Reg16Symbol::HL);
                (h, l)
            }

            // should not reach
            Reg16Symbol::SP => (0, 0),
        };

        (high as u16) << 8 | low as u16
    }

    fn set_r16(&mut self, sym: &Reg16Symbol, val: u16) {
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

            Reg16Symbol::SP => self.sp = val,
        }
    }

    fn inc_r16(&mut self, r16s: Reg16Symbol) -> u8 {
        let curr = self.get_r16(&r16s);
        self.set_r16(&r16s, curr.wrapping_add(1));
        1
    }

    fn dec_r16(&mut self, r16s: Reg16Symbol) -> u8 {
        let curr = self.get_r16(&r16s);
        self.set_r16(&r16s, curr.wrapping_sub(1));
        1
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

    fn stack_push_u8(&mut self, val: u8) -> u8 {
        self.sp = self.sp.wrapping_sub(1);
        self.bus_write(self.sp as usize, val);
        2
    }

    fn stack_push_u16(&mut self, val: u16) -> u8 {
        let low = val as u8;
        let high = (val >> 8) as u8;
        self.stack_push_u8(high);
        self.stack_push_u8(low);
        3
    }

    fn stack_pop_u8(&mut self) -> (u8, u8) {
        let (val, _) = self.bus_read(self.sp as usize);
        self.sp = self.sp.wrapping_add(1);
        (val, 1)
    }

    fn stack_pop_u16(&mut self) -> (u16, u8) {
        let (low, _) = self.stack_pop_u8();
        let (high, _) = self.stack_pop_u8();
        ((high as u16) << 8 | low as u16, 2)
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

    fn val_from_op8(&mut self, op: &Op8) -> (u8, u8) {
        match op {
            Op8::Reg(r8s) => (self.get_r8(&r8s), 0),

            Op8::Addr(r16s) => {
                let val = self.get_r16(&r16s);
                self.bus_read(val as usize)
            }

            Op8::Byte => self.fetch(),

            Op8::AddrBytes => {
                let (addr, c) = self.fetch_2();
                let (byte, c2) = self.bus_read(addr as usize);
                (byte, c + c2)
            }

            Op8::HighByte => {
                let (ofs, c) = self.fetch();
                let ofs_u16 = ofs as u16;
                let addr = ofs_u16 + 0xFF00;
                let (byte, c2) = self.bus_read(addr as usize);
                (byte, c + c2)
            }

            Op8::C => {
                let mut addr = 0xFF00;

                if self.f.c {
                    addr += 1;
                }

                self.bus_read(addr as usize)
            }
        }
    }

    fn write_to_op8(&mut self, op: &Op8, val: u8) -> u8 {
        match op {
            Op8::Reg(r8s) => {
                self.set_r8(&r8s, val);
                0
            }

            Op8::Addr(r16s) => {
                let rval = self.get_r16(&r16s);
                self.bus_write(rval as usize, val)
            }

            Op8::Byte => panic!("Invalid instruction: can't write to Op8::Byte"),

            Op8::AddrBytes => {
                let (addr, c) = self.fetch_2();
                c + self.bus_write(addr as usize, val)
            }

            Op8::HighByte => {
                let (ofs, c) = self.fetch();
                let ofs_u16 = ofs as u16;
                let addr = ofs_u16 + 0xFF00;
                c + self.bus_write(addr as usize, val)
            }

            Op8::C => {
                let mut addr = 0xFF00;

                if self.f.c {
                    addr += 1;
                }

                self.bus_write(addr as usize, val)
            }
        }
    }

    fn val_from_op16(&mut self, op: &Op16) -> (u16, u8) {
        match op {
            Op16::Reg(r16s) => (self.get_r16(&r16s), 0),
            Op16::Bytes => self.fetch_2(),
        }
    }

    fn write_to_op16(&mut self, op: &Op16, val: u16) {
        match op {
            Op16::Reg(r16s) => self.set_r16(&r16s, val),
            Op16::Bytes => panic!("Invalid instruction: can't write to Op16::Bytes"),
        }
    }

    fn check_cond(&self, c: &Condition) -> bool {
        match c {
            Condition::None => true,
            Condition::C => self.f.c,
            Condition::Z => self.f.z,
            Condition::NC => !self.f.c,
            Condition::NZ => !self.f.z,
        }
    }

    // Instructions take a variable amount of CPU cycles
    // from https://emudev.de/gameboy-emulator/opcode-cycles-and-timings/ return the number of
    // cycles executed to develop accurate timing. Yields number of M-cycles.
    fn execute(&mut self, inst: Instruction, logging_file: &mut fs::File) -> u8 {
        let curr_pc = self.pc;
        let mut inst_str = inst.to_string();
        let mut write_to_log = true;

        let cycles = match inst {
            Instruction::NOP => 0,

            Instruction::LD_8_8(dst, src) => {
                let (val, cycles) = self.val_from_op8(&src);

                if let Op8::Byte = src {
                    inst_str = format!("LD {dst} ${:02X}", val)
                }

                cycles + self.write_to_op8(&dst, val)
            }

            Instruction::LD_16_16(dst, src) => {
                let (val, cycles) = self.val_from_op16(&src);

                if let Op16::Bytes = src {
                    inst_str = format!("LD {dst} ${:04X}", val);
                }

                self.write_to_op16(&dst, val);
                cycles
            }

            Instruction::LD_a16_SP => {
                let (addr, _) = self.fetch_2();

                // write SP & 0xFF (low byte) to addr, and SP >> 8 (high byte) to addr + 1
                self.bus_write(addr as usize, (self.sp & 0xFF) as u8);
                self.bus_write((addr + 1) as usize, (self.sp >> 8) as u8);
                4
            }

            // NOTE: should this actually add to sp or no
            Instruction::LD_HL_SP_e8 => {
                self.execute(Instruction::ADD_SP_e8, logging_file);
                self.set_r16(&Reg16Symbol::HL, self.sp);
                2
            }

            Instruction::INC_r16(r16s) => self.inc_r16(r16s),

            Instruction::INC_r8(op) => {
                let (r, cycles) = self.val_from_op8(&op);

                self.f.n = false;

                // check overflow from bit 3 (bits 0-3 are on)
                self.f.h = r & 0xF == 0xF;

                let new_val = r.wrapping_add(1);
                self.f.z = new_val == 0;

                self.write_to_op8(&op, new_val);
                cycles
            }

            Instruction::DEC_r16(r16s) => self.dec_r16(r16s),

            Instruction::DEC_r8(op) => {
                let (r, cycles) = self.val_from_op8(&op);

                self.f.n = true;

                // check if a borrow from bit 4 is required (bits 0-3 are off)
                self.f.h = r & 0x0F == 0;

                let new_val = r.wrapping_sub(1);
                self.f.z = new_val == 0;

                self.write_to_op8(&op, new_val);
                cycles
            }

            Instruction::ADD_HL_r16(r16s) => {
                self.f.n = false;

                let hl = self.get_r16(&Reg16Symbol::HL);
                let r16_val = self.get_r16(&r16s);

                let res = self.sum_u16_with_flags(hl, r16_val);

                self.set_r16(&Reg16Symbol::HL, res);
                1
            }

            Instruction::ADD_A(op) => {
                let (val, cycles) = self.val_from_op8(&op);
                let res = self.sum_u8_with_flags(self.a.get(), val);
                self.a.set(res);
                self.f.z = res == 0;
                self.f.n = false;
                if let Op8::Byte = op {
                    inst_str = format!("ADD A ${:02X}", val);
                }

                cycles
            }

            Instruction::ADD_SP_e8 => {
                let (ofs, _) = self.fetch();
                let ofs_signed = ofs as i8;

                // set h and c flags as u8 addition
                let sp_low = (self.sp & 0xFF) as u8;
                self.f.h = (sp_low & 0x0F) + (ofs & 0x0F) > 0x0F;
                self.f.c = (sp_low as u16) + (ofs as u16) > 0xFF;

                self.sp = self.sp.wrapping_add_signed(ofs_signed.into());

                self.f.z = self.sp == 0;
                self.f.n = false;
                3
            }

            Instruction::ADC_A(op) => {
                let carry = if self.f.c { 1 } else { 0 };
                let (mut val, cycles) = self.val_from_op8(&op);
                val = val.wrapping_add(carry);
                let res = self.sum_u8_with_flags(self.a.get(), val);
                self.a.set(res);
                self.f.z = res == 0;
                self.f.n = false;
                if let Op8::Byte = op {
                    inst_str = format!("ADC A ${:02X}", val);
                }

                cycles
            }

            Instruction::SUB_A(op) => {
                let (val, cycles) = self.val_from_op8(&op);
                let res = self.sub_u8_with_flags(self.a.get(), val);
                self.a.set(res);
                self.f.z = res == 0;
                self.f.n = true;
                if let Op8::Byte = op {
                    inst_str = format!("SUB A ${:02X}", val);
                }

                cycles
            }

            Instruction::SBC_A(op) => {
                let carry = if self.f.c { 1 } else { 0 };
                let (mut val, cycles) = self.val_from_op8(&op);
                val = val.wrapping_add(carry);
                let res = self.sub_u8_with_flags(self.a.get(), val);
                self.a.set(res);
                self.f.z = res == 0;
                self.f.n = true;
                if let Op8::Byte = op {
                    inst_str = format!("SBC A ${:02X}", val);
                }

                cycles
            }

            Instruction::AND_A(op) => {
                let (val, cycles) = self.val_from_op8(&op);
                let res = self.a.get() & val;
                self.a.set(res);
                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = true;
                self.f.c = false;

                if let Op8::Byte = op {
                    inst_str = format!("AND A ${:02X}", val);
                }
                cycles
            }

            Instruction::XOR_A(op) => {
                let (val, cycles) = self.val_from_op8(&op);
                let res = self.a.get() ^ val;
                self.a.set(res);
                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;

                if let Op8::Byte = op {
                    inst_str = format!("XOR A ${:02X}", val);
                }
                cycles
            }

            Instruction::OR_A(op) => {
                let (val, cycles) = self.val_from_op8(&op);
                let res = self.a.get() | val;
                self.a.set(res);
                self.f.z = res == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;

                if let Op8::Byte = op {
                    inst_str = format!("OR A ${:02X}", val);
                }
                cycles
            }

            Instruction::CP_A(op) => {
                let (val, cycles) = self.val_from_op8(&op);
                let res = self.sub_u8_with_flags(self.a.get(), val);
                self.f.z = res == 0;
                self.f.n = true;

                if let Op8::Byte = op {
                    inst_str = format!("CP A ${:02X}", val);
                }
                cycles
            }

            Instruction::JP(cond, src) => {
                let (addr, mut cycles) = self.val_from_op16(&src);
                let checked_cond = self.check_cond(&cond);
                if checked_cond {
                    self.pc = addr;
                    cycles += match src {
                        Op16::Reg(_) => 0,
                        _ => 1,
                    };
                }

                if let Op16::Bytes = src {
                    inst_str = format!("JP{cond} ${:04X}", addr);
                }
                cycles
            }

            Instruction::JR(cond) => {
                let (ofs, mut cycles) = self.fetch();
                let ofs_i8 = ofs as i8;
                let cond = self.check_cond(&cond);
                if cond {
                    self.pc = self.pc.wrapping_add_signed(ofs_i8.into());
                    cycles += 1;
                }
                cycles
            }

            Instruction::RET(cond) => {
                let mut cycles = 1; // condition check is done in a full cycle
                let cond = self.check_cond(&cond);
                if cond {
                    let (val, c) = self.stack_pop_u16();
                    cycles += c;

                    self.pc = val;
                    cycles += 1; // setting pc must be done after stack pop cycles are done
                }
                cycles
            }

            Instruction::RETI => {
                let cycles = self.execute(Instruction::RET(Condition::None), logging_file);
                self.set_ime = false;
                self.ime = true;
                cycles
            }

            Instruction::PUSH_r16(r16s) => {
                let val = self.get_r16(&r16s);
                self.stack_push_u16(val)
            }

            Instruction::POP_r16(r16s) => {
                let (val, cycles) = self.stack_pop_u16();
                self.set_r16(&r16s, val);
                cycles
            }

            Instruction::PUSH_AF => {
                let a = self.get_r8(&Reg8Symbol::A);
                let c1 = self.stack_push_u8(a);

                let f = self.f.to_u8();
                let c2 = self.stack_push_u8(f);

                c1 + c2
            }

            Instruction::POP_AF => {
                let (f_new, mut cycles) = self.stack_pop_u8();
                self.f.set_from_u8(f_new);

                let (a_new, c) = self.stack_pop_u8();
                cycles += c;
                self.set_r8(&Reg8Symbol::A, a_new);

                cycles
            }

            Instruction::CALL(cond) => {
                let (addr, mut cycles) = self.fetch_2();
                let cond = self.check_cond(&cond);
                if cond {
                    cycles += self.stack_push_u16(self.pc);
                    self.pc = addr; // done concurrently with stack push, no extra cycles
                }
                cycles
            }

            Instruction::RST(addr) => {
                let ret_addr = self.pc;
                let cycles = self.stack_push_u16(ret_addr);
                self.pc = addr;
                cycles
            }

            Instruction::RLCA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_left(val);
                self.set_r8(&Reg8Symbol::A, val);
                0
            }

            Instruction::RRCA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_right(val);
                self.set_r8(&Reg8Symbol::A, val);
                0
            }

            Instruction::RLA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_left_through_carry(val);
                self.set_r8(&Reg8Symbol::A, val);
                0
            }

            Instruction::RRA => {
                let mut val = self.get_r8(&Reg8Symbol::A);
                val = self.u8_rot_right_through_carry(val);
                self.set_r8(&Reg8Symbol::A, val);
                0
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

                    self.set_r8(&Reg8Symbol::A, self.get_r8(&Reg8Symbol::A).wrapping_sub(adj));
                } else {
                    if self.f.h || self.get_r8(&Reg8Symbol::A) & 0x0F > 0x09 {
                        adj += 0x06;
                    }

                    if self.f.c || self.get_r8(&Reg8Symbol::A) > 0x99 {
                        adj += 0x60;
                        self.f.c = true;
                    }

                    self.set_r8(&Reg8Symbol::A, self.get_r8(&Reg8Symbol::A).wrapping_add(adj));
                }

                if self.get_r8(&Reg8Symbol::A) == 0 {
                    self.f.z = true;
                }

                0
            }

            Instruction::CPL => {
                self.f.n = true;
                self.f.h = true;

                self.set_r8(&Reg8Symbol::A, !self.get_r8(&Reg8Symbol::A));
                0
            }

            Instruction::SCF => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = true;
                0
            }

            Instruction::CCF => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = !self.f.c;
                0
            }

            Instruction::DI => {
                self.set_ime = false;
                self.ime = false;
                0
            }

            Instruction::EI => {
                self.set_ime = true;
                0
            }

            Instruction::STOP => {
                todo!();
            }

            Instruction::HALT => {
                self.halted = true;
                0
            }

            Instruction::PREFIX => {
                let (op, cycles) = self.fetch();
                let inst = Instruction::decode_prefix(op);
                write_to_log = false;
                cycles + self.execute(inst, logging_file)
            }

            Instruction::RL(op, cir) => {
                let (val, mut cycles) = self.val_from_op8(&op);

                let new = if cir {
                    self.u8_rot_left(val)
                } else {
                    self.u8_rot_left_through_carry(val)
                };
                cycles += 1;

                self.f.z = new == 0;
                self.write_to_op8(&op, new); // writing done concurrently with flag setting

                cycles
            }

            Instruction::RR(op, cir) => {
                let (val, mut cycles) = self.val_from_op8(&op);

                let new = if cir {
                    self.u8_rot_right(val)
                } else {
                    self.u8_rot_right_through_carry(val)
                };
                cycles += 1;

                self.f.z = new == 0;
                self.write_to_op8(&op, new);

                cycles
            }

            Instruction::SLA(op) => {
                let (val, mut cycles) = self.val_from_op8(&op);
                let new = val << 1;
                cycles += 1;

                self.f.z = new == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = val & 128 == 128;
                self.write_to_op8(&op, new);

                cycles
            }

            Instruction::SRA(op) => {
                let (val, mut cycles) = self.val_from_op8(&op);
                let new = val >> 1;
                cycles += 1;

                self.f.z = new == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = val & 1 == 1;
                self.write_to_op8(&op, new);

                cycles
            }

            Instruction::SWAP(op) => {
                let (val, mut cycles) = self.val_from_op8(&op);
                let high = val & 0xF0;
                let low = val & 0x0F;
                let new = (low << 4) | high;
                cycles += 1;

                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                self.write_to_op8(&op, new);

                cycles
            }

            Instruction::SRL(op) => {
                let (val, mut cycles) = self.val_from_op8(&op);
                let new = val >> 1;
                cycles += 1;

                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = val & 1 == 1;
                self.write_to_op8(&op, new);

                cycles
            }

            Instruction::BIT(n, op) => {
                let (mut val, mut cycles) = self.val_from_op8(&op);
                val = val >> n;
                self.f.z = val & 1 == 0;
                self.f.n = false;
                self.f.h = true;
                cycles += 1;

                cycles
            }

            Instruction::RES(n, op) => {
                let (val, mut cycles) = self.val_from_op8(&op);
                let new = val & !(1 << n);
                cycles += 1;
                self.write_to_op8(&op, new);

                cycles
            }

            Instruction::SET(n, op) => {
                let (val, mut cycles) = self.val_from_op8(&op);
                let new = val | (1 << n);
                cycles += 1;
                self.write_to_op8(&op, new);

                cycles
            }

            Instruction::NotImplemented => 4,
        };

        if write_to_log {
            let a = self.a.get();
            let f = self.f.clone();
            let bc = self.get_r16(&Reg16Symbol::BC);
            let de = self.get_r16(&Reg16Symbol::DE);
            let hl = self.get_r16(&Reg16Symbol::HL);
    
            let state = format!(
                "CPU: {:04X}\t{:<24}M: {cycles}\tA: {:02X}\tF: {f}\tBC: {:04X}\tDE: {:04X}\tHL: {:04X}\tSP: {:04X}\n",
                curr_pc, inst_str, a, bc, de, hl, self.sp
            );
            logging_file.write_all(state.as_bytes()).expect("Failed to write to insts log");
        }

        cycles
    }

    fn step(&mut self, logging_file: &mut fs::File) -> u8 {
        if self.halted {
            return 0;
        }

        if self.set_ime {
            self.set_ime = false;
            self.ime = true;
        }

        let mut cycles = self.handle_interrupt();

        let (opcode, c) = self.fetch();
        cycles += c;

        let inst = Instruction::decode(opcode);
        cycles += self.execute(inst, logging_file);

        for _ in 0..4*cycles {
            self.bus.borrow_mut().timer_tick();
        }

        cycles
    }

    pub fn run(&mut self) {
        let logs_path = Path::new("gb_logs");
        if logs_path.exists() {
            fs::remove_dir_all(logs_path).expect("Failed to clear logs");
        }
        fs::create_dir(logs_path).expect("Failed to create logs dir");

        let state_log_path = logs_path.join("cpu_state.log");
        let insts_log_path = logs_path.join("instructions.log");

        let mut state_log = fs::OpenOptions::new()
            .append(true)
            .create(true)
            .open(state_log_path)
            .expect("Failed to open path to state log");
        state_log.write_all(self.export_state().as_bytes()).expect("Failed to write to state log");

        let mut insts_log = fs::OpenOptions::new()
            .append(true)
            .create(true)
            .open(insts_log_path)
            .expect("Failed to open path to insts log");

        let mut dbg_msg = String::new();
        loop {
            self.step(&mut insts_log) as u32;
            state_log.write_all(self.export_state().as_bytes()).expect("Failed to write to state log");

            // Read debug message from blargg test
            if self.bus_read(0xFF02).0 == 0x81 {
                dbg_msg.push(self.bus_read(0xFF01).0 as char);
                println!("DBG: {}", dbg_msg);
                self.bus_write(0xFF02, 0);
            }
        }
    }

    fn handle_interrupt(&mut self) -> u8 {
        if !self.ime {
            return 0;
        }

        let (int_flags, _) = self.bus_read(0xFF0F);
        let (int_enable_flags, _) = self.bus_read(0xFFFF);
        let interrupt = Interrupt::get_first(int_flags, int_enable_flags);
        let handler_addr;
        if let Some(ref t) = interrupt {
            match t {
                Interrupt::VBlank => handler_addr = 0x40,
                Interrupt::LCD => handler_addr = 0x48,
                Interrupt::Timer => handler_addr = 0x50,
                Interrupt::Serial => handler_addr = 0x58,
                Interrupt::Joypad => handler_addr = 0x60,
            }
        } else {
            return 0;
        }

        let mut cycles = 2;  // 2 wait states

        cycles += self.stack_push_u16(self.pc);

        self.pc = handler_addr;
        cycles += 1;

        // unset the interrupt flag
        // this part should be unreachable if interrupt is None
        self.bus_write(0xFF0F, int_flags & !interrupt.as_ref().unwrap().bit_enable());
        self.bus_write(0xFFFF, int_enable_flags & !interrupt.as_ref().unwrap().bit_enable());

        // reset IME after an interrupt is serviced
        self.set_ime = false;
        self.ime = false;

        cycles
    }
}
