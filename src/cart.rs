use core::fmt;

pub struct Cart {
    header: CartHeader,
    data: Vec<u8>,
}

struct CartHeader {
    entry: [u8; 4],
    _nin_logo: [u8; 0x30],
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

#[derive(Debug, Default)]
enum CartBaseType {
    #[default]
    RomOnly,
    MBC1,
    MBC2,
    MMM01,
    MBC3,
    MBC5,
    MBC6,
    MBC7,
    PocketCamera,
    BandaiTama5,
    HuC3,
    HuC1,
}

#[derive(Default)]
struct CartType {
    base_type: CartBaseType,
    ram: bool,
    battery: bool,
    timer: bool,
    rumble: bool,
    sensor: bool,
}

impl CartType {
    fn cart_type_from_byte(byte: u8) -> Option<CartType> {
        match byte {
            0x00 => Some(CartType {
                ..Default::default()
            }),
            0x01 => Some(CartType {
                base_type: CartBaseType::MBC1,
                ..Default::default()
            }),
            0x02 => Some(CartType {
                base_type: CartBaseType::MBC1,
                ram: true,
                ..Default::default()
            }),
            0x03 => Some(CartType {
                base_type: CartBaseType::MBC1,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            0x05 => Some(CartType {
                base_type: CartBaseType::MBC2,
                ..Default::default()
            }),
            0x06 => Some(CartType {
                base_type: CartBaseType::MBC2,
                battery: true,
                ..Default::default()
            }),
            0x0B => Some(CartType {
                base_type: CartBaseType::MMM01,
                ..Default::default()
            }),
            0x0C => Some(CartType {
                base_type: CartBaseType::MMM01,
                ram: true,
                ..Default::default()
            }),
            0x0D => Some(CartType {
                base_type: CartBaseType::MMM01,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            0x0F => Some(CartType {
                base_type: CartBaseType::MBC3,
                timer: true,
                battery: true,
                ..Default::default()
            }),
            0x10 => Some(CartType {
                base_type: CartBaseType::MBC3,
                timer: true,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            0x11 => Some(CartType {
                base_type: CartBaseType::MBC3,
                ..Default::default()
            }),
            0x12 => Some(CartType {
                base_type: CartBaseType::MBC3,
                ram: true,
                ..Default::default()
            }),
            0x13 => Some(CartType {
                base_type: CartBaseType::MBC3,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            0x19 => Some(CartType {
                base_type: CartBaseType::MBC5,
                ..Default::default()
            }),
            0x1A => Some(CartType {
                base_type: CartBaseType::MBC5,
                ram: true,
                ..Default::default()
            }),
            0x1B => Some(CartType {
                base_type: CartBaseType::MBC5,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            0x1C => Some(CartType {
                base_type: CartBaseType::MBC5,
                rumble: true,
                ..Default::default()
            }),
            0x1D => Some(CartType {
                base_type: CartBaseType::MBC5,
                rumble: true,
                ram: true,
                ..Default::default()
            }),
            0x1E => Some(CartType {
                base_type: CartBaseType::MBC5,
                rumble: true,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            0x20 => Some(CartType {
                base_type: CartBaseType::MBC6,
                ..Default::default()
            }),
            0x22 => Some(CartType {
                base_type: CartBaseType::MBC7,
                sensor: true,
                rumble: true,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            0xFC => Some(CartType {
                base_type: CartBaseType::PocketCamera,
                ..Default::default()
            }),
            0xFD => Some(CartType {
                base_type: CartBaseType::BandaiTama5,
                ..Default::default()
            }),
            0xFE => Some(CartType {
                base_type: CartBaseType::HuC3,
                ..Default::default()
            }),
            0xFF => Some(CartType {
                base_type: CartBaseType::HuC1,
                ram: true,
                battery: true,
                ..Default::default()
            }),
            _ => None,
        }
    }
}

impl fmt::Debug for CartType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.base_type)?;
        if self.ram {
            write!(f, "+RAM")?;
        }
        if self.battery {
            write!(f, "+BATTERY")?;
        }
        if self.timer {
            write!(f, "+TIMER")?;
        }
        if self.rumble {
            write!(f, "+RUMBLE")?;
        }
        if self.sensor {
            write!(f, "+SENSOR")?;
        }
        Ok(())
    }
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
            data: rom.to_vec(),
        }
    }

    pub fn print_header(&self) {
        self.header.print_header();
    }

    pub fn cart_read(&self, address: usize) -> u8 {
        self.data[address]
    }

    pub fn cart_write(&mut self, address: usize, value: u8) {
        self.data[address] = value;
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
            entry,
            _nin_logo: nin_logo,
            title,
            cgb_flag: cgb_flag_from_byte(rom[0x143])
                .expect(&format!("No cgb flag mapped for {:X?}", rom[0x143])),
            new_lic_code: ((rom[0x144] as u16) << 8) | rom[0x145] as u16,
            sgb_flag: sgb_flag_from_byte(rom[0x146]),
            cart_type: CartType::cart_type_from_byte(rom[0x147])
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
