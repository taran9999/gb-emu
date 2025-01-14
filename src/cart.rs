struct CartHeader {
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
        CartHeader {
            entry: rom[0x100..0x103].try_into().unwrap(),
            nin_logo: rom[0x104..0x133].try_into().unwrap(),
            title: rom[0x134..0x143].try_into().unwrap(),
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
}