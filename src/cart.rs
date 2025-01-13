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