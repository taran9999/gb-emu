use super::*;
use crate::bus::Bus;
use crate::cart::Cart;

macro_rules! setup {
    () => {
        let rom = [0u8; 32 * 1024];
        let mut cart = Cart::read_rom(&rom);
        let mut bus = Bus::new(&mut cart);
        let mut cpu = CPU::init(&mut bus);
    };
}

#[test]
fn random_test() {
    setup!();
    assert!(true);
}
