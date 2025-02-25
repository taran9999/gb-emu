pub struct Cart {
    header: CartHeader,
}

struct CartHeader {
    entry: [u8; 4],
    nin_logo: [u8; 0x30],
    title: [u8; 0x10],
    cgb_flag: CGBFlag,
    new_lic_code: u16,
    sgb_flag: SGBFlag,
    cart_type: CartType,
    rom_size: usize,
    ram_size: usize,
    destination_code: DestCode,
    old_lic_code: u8,
    rom_version: u8,
    header_checksum: u8,
    global_checksum: u16,
}

#[derive(Debug)]
enum CGBFlag {
    NoCGB,
    CGBAndMono,
    CGBOnly,
}

#[derive(Debug)]
enum SGBFlag {
    NotSupported,
    Supported,
}

#[derive(Debug)]
enum CartType {
    RomOnly,
    MBC1,
    MBC1Ram,
    MBC1RamBattery,
    MBC2,
    MBC2Battery,
    RomRam,
    RomRamBattery,
    MMM01,
    MMM01Ram,
    MMM01RamBattery,
    MBC3TimerBattery,
    MBC3TimerRamBattery,
    MBC3,
    MBC3Ram,
    MBC3RamBattery,
    MBC5,
    MBC5Ram,
    MBC5RamBattery,
    MBC5Rumble,
    MBC5RumbleRam,
    MBC5RumbleRamBattery,
    MBC6,
    MBC7SensorRumbleRamBattery,
    PocketCamera,
    BandaiTama5,
    HuC3,
    HuC1RamBattery,
}

#[derive(Debug)]
enum DestCode {
    Japan,
    Overseas,
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

fn cgb_flag_from_byte(byte: u8) -> Option<CGBFlag> {
    match byte {
        0x00 => Some(CGBFlag::NoCGB),
        0x80 => Some(CGBFlag::CGBAndMono),
        0xC0 => Some(CGBFlag::CGBOnly),
        _ => None,
    }
}

fn sgb_flag_from_byte(byte: u8) -> SGBFlag {
    if byte == 0x03 {
        SGBFlag::Supported
    } else {
        SGBFlag::NotSupported
    }
}

fn cart_type_from_byte(byte: u8) -> Option<CartType> {
    match byte {
        0x00 => Some(CartType::RomOnly),
        0x01 => Some(CartType::MBC1),
        0x02 => Some(CartType::MBC1Ram),
        0x03 => Some(CartType::MBC1RamBattery),
        0x05 => Some(CartType::MBC2),
        0x06 => Some(CartType::MBC2Battery),
        0x08 => Some(CartType::RomRam),
        0x09 => Some(CartType::RomRamBattery),
        0x0B => Some(CartType::MMM01),
        0x0C => Some(CartType::MMM01Ram),
        0x0D => Some(CartType::MMM01RamBattery),
        0x0F => Some(CartType::MBC3TimerBattery),
        0x10 => Some(CartType::MBC3TimerRamBattery),
        0x11 => Some(CartType::MBC3),
        0x12 => Some(CartType::MBC3Ram),
        0x13 => Some(CartType::MBC3RamBattery),
        0x19 => Some(CartType::MBC5),
        0x1A => Some(CartType::MBC5Ram),
        0x1B => Some(CartType::MBC5RamBattery),
        0x1C => Some(CartType::MBC5Rumble),
        0x1D => Some(CartType::MBC5RumbleRam),
        0x1E => Some(CartType::MBC5RumbleRamBattery),
        0x20 => Some(CartType::MBC6),
        0x22 => Some(CartType::MBC7SensorRumbleRamBattery),
        0xFC => Some(CartType::PocketCamera),
        0xFD => Some(CartType::BandaiTama5),
        0xFE => Some(CartType::HuC3),
        0xFF => Some(CartType::HuC1RamBattery),
        _ => None,
    }
}

fn rom_size_from_byte(byte: u8) -> Option<usize> {
    match byte {
        0x00 => Some(32 * 1024),  // 32 KiB
        0x01 => Some(64 * 1024),  // 64 KiB
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
        _ => None,
    }
}

fn ram_size_from_byte(byte: u8) -> Option<usize> {
    match byte {
        0x00 => Some(0),
        0x01 => Some(2 * 1024), // 2 KiB according to the pandocs footnote, however this flag is actually unused
        0x02 => Some(8 * 1024),
        0x03 => Some(32 * 1024),
        0x04 => Some(128 * 1024),
        0x05 => Some(64 * 1024),
        _ => None,
    }
}

fn dest_code_from_byte(byte: u8) -> Option<DestCode> {
    match byte {
        0x00 => Some(DestCode::Japan),
        0x01 => Some(DestCode::Overseas),
        _ => None,
    }
}
impl Cart {
    pub fn read_rom(rom: &[u8]) -> Cart {
        Cart {
            header: CartHeader::read_rom(rom),
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
            cgb_flag: cgb_flag_from_byte(rom[0x143])
                .expect(&format!("No cgb flag mapped for {:X?}", rom[0x143])),
            new_lic_code: ((rom[0x144] as u16) << 8) | rom[0x145] as u16,
            sgb_flag: sgb_flag_from_byte(rom[0x146]),
            cart_type: cart_type_from_byte(rom[0x147])
                .expect(&format!("No cart type mapped for {:X?}", rom[0x147])),
            rom_size: rom_size_from_byte(rom[0x148])
                .expect(&format!("No rom size mapped for {:X?}", rom[0x148])),
            ram_size: ram_size_from_byte(rom[0x149])
                .expect(&format!("No ram size mapped for {:X?}", rom[0x149])),
            destination_code: dest_code_from_byte(rom[0x14A])
                .expect(&format!("No dest code mapped for {:X?}", rom[0x14A])),
            old_lic_code: rom[0x14B],
            rom_version: rom[0x14C],
            header_checksum: rom[0x14D],
            global_checksum: ((rom[0x14E] as u16) << 8) | rom[0x14F] as u16,
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
