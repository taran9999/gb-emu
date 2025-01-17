use std::fmt::format;

pub struct Cart {
    header: CartHeader
}

struct CartHeader {
    entry: [u8; 4],
    nin_logo: [u8; 0x30],
    title: [u8; 0x10],
    cgb_flag: u8,
    new_lic_code: u16,
    sgb_flag: u8,
    cart_type: u8,
    rom_size: usize,
    ram_size: u8,
    destination_code: u8,
    old_lic_code: u8,
    rom_version: u8,
    header_checksum: u8,
    global_checksum: u16
}

fn ascii_str_from_bytes(bytes: &[u8]) -> String {
    let mut s = String::new();
    for &b in bytes {
        if b.is_ascii_uppercase() || b.is_ascii_digit() {
            s.push(b as char);
        }
    }
    s
}

fn rom_size_from_byte(byte: u8) -> Option<usize> {
    match byte {
        0x00 => Some(32 * 1024), // 32 KiB
        0x01 => Some(64 * 1024), // 64 KiB
        0x02 => Some(128 * 1024), // 128 KiB
        0x03 => Some(256 * 1024),
        0x04 => Some(512 * 1024),
        0x05 => Some(1024 * 1024), // 1 MiB
        0x06 => Some(2 * 1024 * 1024),
        0x07 => Some(4 * 1024 * 1024),
        0x08 => Some(8 * 1024 * 1024),
        0x52 => Some((1.1 * 1048576 as f32) as usize),
        0x53 => Some((1.2 * 1048576 as f32) as usize),
        0x54 => Some((1.5 * 1048576 as f32) as usize),
        _ => None
    }
}

impl Cart {
    pub fn read_rom(rom: &[u8]) -> Cart {
        Cart {
            header: CartHeader::read_rom(rom)
        }
    }

    pub fn print_header(&self) {
        self.header.print_header();
    }
}

impl CartHeader {
    fn read_rom(rom: &[u8]) -> CartHeader {
        let mut entry = [0u8; 4];
        entry.copy_from_slice(&rom[0x100..0x104]);
        let mut nin_logo = [0u8; 48];
        nin_logo.copy_from_slice(&rom[0x104..0x134]);
        let mut title = [0u8; 16];
        title.copy_from_slice(&rom[0x134..0x144]);

        CartHeader {
            entry: entry,
            nin_logo: nin_logo,
            title: title,
            cgb_flag: rom[0x143],
            new_lic_code: ((rom[0x144] as u16) << 8) | rom[0x145] as u16,
            sgb_flag: rom[0x146],
            cart_type: rom[0x147],
            rom_size: rom_size_from_byte(rom[0x148]).expect(&format!("No rom size mapped for {:X?}", rom[0x148])),
            ram_size: rom[0x149],
            destination_code: rom[0x14A],
            old_lic_code: rom[0x14B],
            rom_version: rom[0x14C],
            header_checksum: rom[0x14D],
            global_checksum: ((rom[0x14E] as u16) << 8) | rom[0x14F] as u16
        }
    }

    fn print_header(&self) {
        println!("Cart Header     :");
        println!("entry           : {:X?}", self.entry);
        println!("title           : {:X?}", ascii_str_from_bytes(&self.title));
        println!("cgb flag        : {:X?}", self.cgb_flag);
        println!("new lic code    : {:X?}", self.new_lic_code);
        println!("sgb flag        : {:X?}", self.sgb_flag);
        println!("cart type       : {:X?}", self.cart_type);
        println!("rom size        : {:X?}", self.rom_size);
        println!("ram size        : {:X?}", self.ram_size);
        println!("destination code: {:X?}", self.destination_code);
        println!("old lic code    : {:X?}", self.old_lic_code);
        println!("rom version     : {:X?}", self.rom_version);
        println!("header checksum : {:X?}", self.header_checksum);
        println!("global checksum : {:X?}", self.global_checksum);
    }
}