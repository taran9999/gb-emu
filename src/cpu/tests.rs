use super::*;
use crate::bus::Bus;
use crate::cart::Cart;

macro_rules! setup {
    ($($addr:expr => $value:expr),*) => {{
        let mut rom = [0u8; 32 * 1024];
        $( rom[$addr] = $value; )*

        let mut cpu = {
            let mut cart = Cart::read_rom(&rom);
            let mut bus = Bus::new(&mut cart);
            CPU::init(&mut bus);
        };

        (rom, cpu)
    }};
}

#[test]
fn random_test() {
    let (rom, cpu) = setup!(0x1000 => 0xAB);
    assert!(rom[0x1000] == 0xAB)
}
