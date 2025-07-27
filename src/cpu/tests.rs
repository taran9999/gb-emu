use super::*;
use crate::bus::Bus;
use crate::cart::Cart;

// macro_rules! setup {
//     ($($addr:expr => $value:expr),*) => {{
//         let mut rom = [0u8; 32 * 1024];
//         $( rom[$addr] = $value; )*
//
//         let mut cpu = {
//             let mut cart = Cart::read_rom(&rom);
//             let mut bus = Bus::new(&mut cart);
//             CPU::init(&mut bus)
//         };
//
//         cpu
//     }};
// }

#[test]
fn test_add_a_r8() {
    let mut rom = [0u8; 32 * 1024];
    rom[0x100] = 0x80;

    let mut cart = Cart::read_rom(&rom);
    let mut bus = Bus::new(&mut cart);
    let mut cpu = CPU::init(&mut bus);

    cpu.a.0 = 1;
    cpu.b.0 = 1;

    cpu.step();

    let z = cpu.get_flag_z();
    let n = cpu.get_flag_n();
    let h = cpu.get_flag_h();
    let c = cpu.get_flag_c();

    assert!(
        cpu.a.0 == 2,
        "Incorrect value in register a: expected {}, got {}.",
        2,
        cpu.a.0
    );

    assert!(
        !z,
        "Incorrect value for flag z: expected {}, got {}",
        false, z
    );

    assert!(
        !n,
        "Incorrect value for flag z: expected {}, got {}",
        false, n
    );

    assert!(
        !h,
        "Incorrect value for flag z: expected {}, got {}",
        false, h
    );

    assert!(
        !c,
        "Incorrect value for flag z: expected {}, got {}",
        false, c
    );
}
