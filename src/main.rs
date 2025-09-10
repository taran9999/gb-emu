use std::fs::{self, File};
use std::io::{self, Read, Write};

use bus::Bus;
use cart::Cart;
use cpu::CPU;
use gbio::Io;
use ram::Ram;

mod bus;
mod cart;
mod cpu;
mod gbio;
mod instruction;
mod ram;

fn main() {
    // read a directory for file selection
    // currently hardcoded to a "roms" directory where the the project is being run from
    let roms: Vec<_> = fs::read_dir("roms")
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

    let mut io = Io::new();
    let mut ram = Ram::new();
    let mut bus = Bus::new(&mut ch, &mut io, &mut ram);
    let mut cpu = CPU::init(&mut bus);

    for _ in 0..10 {
        cpu.step();
    }
    println!("{}", cpu.export_state())
}
