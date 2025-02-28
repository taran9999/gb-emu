use crate::cart::Cart;

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

struct Bus {
    cart: Cart,
}

impl Bus {
    fn new(cart: Cart) -> Bus {
        Bus { cart }
    }

    fn read(&self, address: usize) {
        if address <= 0x8000 {
            self.cart.cart_read(address);
        }
    }

    fn write(&mut self, address: usize, value: u8) {
        if address <= 0x8000 {
            self.cart.cart_write(address, value);
        }
    }
}
