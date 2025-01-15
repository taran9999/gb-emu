pub struct CartHeader {
    entry: [u8; 4],
    nin_logo: [u8; 0x30],
    title: [u8; 0x10],
    cgb_flag: u8,
    new_lic_code: u16,
    sgb_flag: u8,
    cart_type: u8,
    rom_size: u8,
    ram_size: u8,
    destination_code: u8,
    old_lic_code: u8,
    rom_version: u8,
    header_checksum: u8,
    global_checksum: u16
}

impl CartHeader {
    pub fn read_rom(rom: &[u8]) -> CartHeader {
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
            rom_size: rom[0x148],
            ram_size: rom[0x149],
            destination_code: rom[0x14A],
            old_lic_code: rom[0x14B],
            rom_version: rom[0x14C],
            header_checksum: rom[0x14D],
            global_checksum: ((rom[0x14E] as u16) << 8) | rom[0x14F] as u16
        }
    }

    pub fn print_header(&self) {
        println!("Cart Header     :");
        println!("entry           : {:X?}", self.entry);
        println!("title           : {:X?}", self.title);
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