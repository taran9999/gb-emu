use super::*;
use crate::bus::Bus;
use crate::cart::Cart;

// TODO: this macro doesn't work because the cart and bus can't live long enough if created inside
// (i assumed these were C style macros until i tried this)
// see if there is a way to avoid copy pasting the cpu setup in test cases

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

fn ld_r8_r8(src_reg: Reg8Symbol, target_reg: Reg8Symbol, src_init: u8) {
    let opcode_base = match target_reg {
        Reg8Symbol::B => 0x40,
        Reg8Symbol::C => 0x48,
        Reg8Symbol::D => 0x50,
        Reg8Symbol::E => 0x58,
        Reg8Symbol::H => 0x60,
        Reg8Symbol::L => 0x68,
        Reg8Symbol::A => 0x78,
        Reg8Symbol::F => panic!("No opcode with F src reg"),
    };

    let opcode = opcode_base
        + match src_reg {
            Reg8Symbol::B => 0,
            Reg8Symbol::C => 1,
            Reg8Symbol::D => 2,
            Reg8Symbol::E => 3,
            Reg8Symbol::H => 4,
            Reg8Symbol::L => 5,
            Reg8Symbol::A => 7,
            Reg8Symbol::F => panic!("No opcode with F target reg"),
        };

    let mut rom = [0u8; 32 * 1024];
    rom[0x100] = opcode;

    let mut cart = Cart::read_rom(&rom);
    let mut bus = Bus::new(&mut cart);
    let mut cpu = CPU::init(&mut bus);

    let src = cpu.reg8_from_symbol(&src_reg);
    src.0 = src_init;

    cpu.step();

    let src_val = cpu.reg8_from_symbol(&src_reg).0;
    let target_val = cpu.reg8_from_symbol(&target_reg).0;

    assert!(
        src_val == src_init,
        "Source register lost its value: expected {}, got {}",
        src_init,
        src_val
    );
    assert!(
        target_val == src_val,
        "Target register value does not match source, expected {}, got {}",
        src_val,
        target_val
    );
}

fn add_a_r8(
    target_reg: Reg8Symbol,
    a_init: u8,
    target_init: u8,
    a_expected: u8,
    z_expected: bool,
    n_expected: bool,
    h_expected: bool,
    c_expected: bool,
) {
    let opcode = match target_reg {
        Reg8Symbol::B => 0x80,
        Reg8Symbol::C => 0x81,
        Reg8Symbol::D => 0x82,
        Reg8Symbol::E => 0x83,
        Reg8Symbol::H => 0x84,
        Reg8Symbol::L => 0x85,
        Reg8Symbol::A => 0x87,
        Reg8Symbol::F => panic!("No opcode for ADD A F"),
    };

    let mut rom = [0u8; 32 * 1024];
    rom[0x100] = opcode;

    let mut cart = Cart::read_rom(&rom);
    let mut bus = Bus::new(&mut cart);
    let mut cpu = CPU::init(&mut bus);

    cpu.a.0 = a_init;
    let target = cpu.reg8_from_symbol(&target_reg);
    target.0 = target_init;

    cpu.step();

    let z = cpu.get_flag_z();
    let n = cpu.get_flag_n();
    let h = cpu.get_flag_h();
    let c = cpu.get_flag_c();

    assert!(
        cpu.a.0 == a_expected,
        "Incorrect value in register a: expected {}, got {}.",
        a_expected,
        cpu.a.0
    );

    assert!(
        z == z_expected,
        "Incorrect value for flag z: expected {}, got {}",
        z_expected,
        z
    );

    assert!(
        n == n_expected,
        "Incorrect value for flag n: expected {}, got {}",
        n_expected,
        n
    );

    assert!(
        h == h_expected,
        "Incorrect value for flag h: expected {}, got {}",
        h_expected,
        h
    );

    assert!(
        c == c_expected,
        "Incorrect value for flag c: expected {}, got {}",
        c_expected,
        c
    );
}

// LD_r8_r8
#[test]
fn test_ld_r8_r8() {
    ld_r8_r8(Reg8Symbol::A, Reg8Symbol::B, 1);
}

#[test]
fn test_ld_r8_r8_self() {
    ld_r8_r8(Reg8Symbol::A, Reg8Symbol::A, 1);
}

// ADD_A_r8
#[test]
fn test_add_a_r8_no_carry() {
    add_a_r8(Reg8Symbol::B, 1, 1, 2, false, false, false, false);
}

#[test]
fn test_add_a_r8_half_carry() {
    add_a_r8(Reg8Symbol::B, 1, 15, 16, false, false, true, false);
}

#[test]
fn test_add_a_r8_half_carry_2() {
    add_a_r8(Reg8Symbol::B, 15, 12, 27, false, false, true, false);
}

#[test]
fn test_add_a_r8_full_carry_zero() {
    add_a_r8(Reg8Symbol::B, 1, 255, 0, true, false, true, true);
}

#[test]
fn test_add_a_r8_full_carry() {
    add_a_r8(Reg8Symbol::B, 10, 250, 4, false, false, true, true);
}

#[test]
fn test_add_a_r8_no_carry_zero() {
    add_a_r8(Reg8Symbol::B, 0, 0, 0, true, false, false, false);
}
