use std::env;
use std::fs::File;
use std::io::Read;

use bus::Bus;
use cart::Cart;
use cpu::CPU;

mod bus;
mod cart;
mod cpu;

fn main() {
    let args: Vec<String> = env::args().collect();
    let fpath = &args[1];
    println!("fpath: {fpath}");

    // let mut test_header_rom: [u8; 336] = [0; 336];
    // let ch = cart::CartHeader::read_rom(&test_header_rom);

    let mut file = File::open(&format!("roms/{fpath}")).expect("Error opening file.");
    let mut file_buffer = Vec::new();
    file.read_to_end(&mut file_buffer)
        .expect("Error reading file.");
    let mut ch = Cart::read_rom(&file_buffer);

    ch.print_header();

    let mut bus = Bus::new(&mut ch);
    let mut cpu = CPU::init(&mut bus);

    for i in 0..5 {
        let byte = cpu.fetch();
        println!("Byte: {:X}", byte);
    }
}
