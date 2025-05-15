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
            _ => {
                println!("Warning: opcode {:X} not implemented", opcode);
                4
            }
        }
    }
}
