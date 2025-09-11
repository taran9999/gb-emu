use crate::cart::Cart;
use crate::gbio::Io;
use crate::ram::Ram;

/* Memory map:
0000 - 3FFF: 16 KiB fixed bank
4000 - 7FFF: 16 KiB switchable bank
8000 - 9FFF: 8 KiB Video Ram
A000 - BFFF: 8 KiB External RAM
C000 - CFFF: 4 KiB Work RAM
D000 - DFFF: 4 KiB Work RAM, switchable bank 1-7 in CGB mode
E000 - FDFF: Not usable
FE00 - FE9F: Object attribute memory
FEA0 - FEFF: Not usable
FF00 - FF7F: IO Registers
FF80 - FFFE: High RAM
FFFF: Interrupt Enable register
*/

pub struct Bus<'a> {
    cart: &'a mut Cart,
    io: &'a mut Io,
    ram: &'a mut Ram,
}

impl Bus<'_> {
    pub fn new<'a>(cart: &'a mut Cart, io: &'a mut Io, ram: &'a mut Ram) -> Bus<'a> {
        Bus { cart, io, ram }
    }

    pub fn read(&self, address: usize) -> u8 {
        match address {
            0x0000..=0x7FFF => self.cart.cart_read(address),
            0xC000..=0xDFFF => self.ram.wram_read(address),
            0xFF00..=0xFF7F => self.io.io_read(address),
            0xFF80..=0xFFFE => self.ram.hram_read(address),

            _ => {
                println!("(Warning): read from unknown area {:X}", address);
                0xFF
            }
        }
    }

    pub fn write(&mut self, address: usize, value: u8) {
        println!("Bus: Write ${:04X} <- ${:02X}", address, value);

        match address {
            0x0000..=0x7FFF => self.cart.cart_write(address, value),
            0xC000..=0xDFFF => self.ram.wram_write(address, value),
            0xFF00..=0xFF7F => self.io.io_write(address, value),
            0xFF80..=0xFFFE => self.ram.hram_write(address, value),

            _ => println!("(Warning): write to unknown area {:X}", address),
        }
    }
}
