use std::cell::RefCell;
use std::fs::{self, File};
use std::io::{self, Read, Write};

use apu::APU;
use bus::Bus;
use cart::Cart;
use cpu::CPU;
use gbio::Io;
use ram::Ram;
use timer::Timer;

mod apu;
mod bus;
mod cart;
mod cpu;
mod gbio;
mod instruction;
mod interrupts;
mod ram;
mod timer;

fn main() {
    // read a directory for file selection
    // currently hardcoded to a "roms" directory where the the project is being run from
    let roms: Vec<_> = fs::read_dir("tests")
        .expect("Error reading roms directory")
        .filter_map(|f| f.ok())
        .filter(|f| {
            let path = f.path();
            if !path.is_file() {
                return false;
            }
            match path.extension().and_then(|ext| ext.to_str()) {
                Some("gb") | Some("gbc") => true,
                _ => false,
            }
        })
        .collect();

    println!("Found roms:");
    for (i, f) in roms.iter().enumerate() {
        if let Some(fname) = f.path().file_name().and_then(|os_str| os_str.to_str()) {
            println!("{}. {}", i + 1, fname);
        }
    }

    print!("Select rom by number: ");
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    let fpath = match input.trim().parse::<usize>() {
        Ok(n) if n >= 1 && n <= roms.len() => Some(roms[n - 1].path()),
        _ => {
            println!("Invalid selection.");
            None
        }
    };

    let mut file =
        File::open(fpath.expect("Could not retrieve file path.")).expect("Error opening file.");
    let mut file_buffer = Vec::new();
    file.read_to_end(&mut file_buffer)
        .expect("Error reading file.");
    let mut ch = Cart::read_rom(&file_buffer);

    ch.print_header();

    let mut timer = Timer::new();
    let mut apu = APU::new();
    let mut io = Io::new(&mut timer, &mut apu);
    let mut ram = Ram::new();
    let bus = RefCell::new(Bus::new(&mut ch, &mut io, &mut ram));
    let mut cpu = CPU::init(&bus);

    cpu.run();
}
