mod cart;

fn main() {
    let mut test_header_rom: [u8; 336] = [0; 336];
    let ch = cart::CartHeader::read_rom(&test_header_rom);
    ch.print_header();
}
