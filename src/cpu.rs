// separate registers into their 8-bit components, but allow methods to read/write as either 8 or
// 16 bit
struct Registers {
    a: u8,
    f: u8,
    b: u8,
    c: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
}
