use std::fmt;

#[derive(Debug)]
pub enum Reg8Symbol {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Reg16Symbol {
    BC,
    DE,
    HL,
    HLI,
    HLD,
    SP,
}

// operand sources for instructions types for which a u8 can be retrieved
pub enum Op8 {
    Reg(Reg8Symbol),
    Addr(Reg16Symbol), // bus read/write with address set to the value in r16
    Byte,              // fetch a byte (cannot be used for writing)
    AddrBytes,         // bus read/write with address determined by fetching 2 bytes
    HighByte,          // fetch a byte, add to 0xFF00, use that address
    C,                 // add carry flag to 0xFF00, use that address
}

impl fmt::Display for Op8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "{:?}", r),
            Self::Addr(r) => write!(f, "{:?}", r),
            Self::Byte => write!(f, "n8"),
            Self::AddrBytes => write!(f, "n16"),
            Self::HighByte => write!(f, "H-n8"),
            Self::C => write!(f, "H-C"),
        }
    }
}

pub enum Op16 {
    Reg(Reg16Symbol),
    Bytes,
}

impl fmt::Display for Op16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "{:?}", r),
            Self::Bytes => write!(f, "n16"),
        }
    }
}

#[derive(Debug)]
pub enum Condition {
    None,
    C,
    Z,
    NC,
    NZ,
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, ""),
            v => write!(f, " {:?}", v),
        }
    }
}

#[allow(non_camel_case_types)]
pub enum Instruction {
    NOP,

    // dst, src
    LD_8_8(Op8, Op8),
    LD_16_16(Op16, Op16),

    // 8 8
    // LD_r8_r8(Reg8Symbol, Reg8Symbol), -- Reg Reg T=4
    // LD_r8_n8(Reg8Symbol), -- Reg Byte T=8
    // LD_r8_r16(Reg8Symbol, Reg16Symbol), -- Reg Addr T=8
    // LD_r16_r8(Reg16Symbol, Reg8Symbol), -- Addr Reg T=8
    // LD_a16_A, -- AddrBytes Reg(A) T=16
    // LD_A_a16, -- Reg(A) AddrBytes T=16
    // LD_HL_n8, -- Addr(HL) Byte T=12
    // LDH_n8_A, -- HighByte Reg(A) T=12
    // LDH_A_n8, -- Reg(A) HighByte T=12
    // LDH_C_A, -- C Reg(A) T=8
    // LDH_A_C, -- Reg(A) C T=8

    // 16 16
    // LD_r16_n16(Reg16Symbol), -- Reg Bytes T=12
    // LD_SP_HL, -- Reg(SP) Reg(HL) T=8
    LD_a16_SP,
    LD_HL_SP_e8,

    INC_r16(Reg16Symbol),
    INC_r8(Reg8Symbol),

    DEC_r16(Reg16Symbol),
    DEC_r8(Reg8Symbol),

    ADD_HL_r16(Reg16Symbol),
    ADD_A(Op8),
    ADD_SP_e8,

    ADC_A(Op8),
    SUB_A(Op8),
    SBC_A(Op8),
    AND_A(Op8),
    XOR_A(Op8),
    OR_A(Op8),
    CP_A(Op8),

    // condition, addr source
    JP(Condition, Op16),
    // JP_n16, -- None Bytes T=16
    // JP_n16_Conditional, -- [] Bytes T=16/12
    // JP_HL, -- None Reg(HL) T=4
    JR(Condition),
    // JR_n16, -- None T=12
    // JR_n16_Conditional(FlagSymbol, bool), -- [] T=12/8
    RET(Condition),
    // RET, T=16
    // RET_Conditional(FlagSymbol, bool), T=20/8
    RETI,
    PUSH_r16(Reg16Symbol),
    POP_r16(Reg16Symbol),
    PUSH_AF,
    POP_AF,
    CALL(Condition),
    // CALL_n16, T=24
    // CALL_n16_Conditional(FlagSymbol, bool), T=24/12
    RST(u16),

    RLCA,
    RRCA,
    RLA,
    RRA,
    DAA,
    CPL,
    SCF,
    CCF,
    DI,
    EI,
    STOP,
    HALT,

    PREFIX,

    // bool for circular variant (RLC)
    RL(Op8, bool),
    RR(Op8, bool),

    SLA(Op8),
    SRA(Op8),
    SWAP(Op8),
    SRL(Op8),

    // use u8 as u3 - values should range from 0 to 7
    BIT(u8, Op8),
    RES(u8, Op8),
    SET(u8, Op8),

    NotImplemented,
}

impl Instruction {
    pub fn decode(opcode: u8) -> Instruction {
        match opcode {
            0x00 => Instruction::NOP,
            0x01 => Instruction::LD_16_16(Op16::Reg(Reg16Symbol::BC), Op16::Bytes),
            0x02 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::BC), Op8::Reg(Reg8Symbol::A)),
            0x03 => Instruction::INC_r16(Reg16Symbol::BC),
            0x04 => Instruction::INC_r8(Reg8Symbol::B),
            0x05 => Instruction::DEC_r8(Reg8Symbol::B),
            0x06 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Byte),
            0x07 => Instruction::RLCA,
            0x08 => Instruction::LD_a16_SP,
            0x09 => Instruction::ADD_HL_r16(Reg16Symbol::BC),
            0x0A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Addr(Reg16Symbol::BC)),
            0x0B => Instruction::DEC_r16(Reg16Symbol::BC),
            0x0C => Instruction::INC_r8(Reg8Symbol::C),
            0x0D => Instruction::DEC_r8(Reg8Symbol::C),
            0x0E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Byte),
            0x0F => Instruction::RRCA,

            0x10 => Instruction::STOP,
            0x11 => Instruction::LD_16_16(Op16::Reg(Reg16Symbol::DE), Op16::Bytes),
            0x12 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::DE), Op8::Reg(Reg8Symbol::A)),
            0x13 => Instruction::INC_r16(Reg16Symbol::DE),
            0x14 => Instruction::INC_r8(Reg8Symbol::D),
            0x15 => Instruction::DEC_r8(Reg8Symbol::D),
            0x16 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Byte),
            0x17 => Instruction::RLA,
            0x18 => Instruction::JR(Condition::None),
            0x19 => Instruction::ADD_HL_r16(Reg16Symbol::DE),
            0x1A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Addr(Reg16Symbol::DE)),
            0x1B => Instruction::DEC_r16(Reg16Symbol::DE),
            0x1C => Instruction::INC_r8(Reg8Symbol::E),
            0x1D => Instruction::DEC_r8(Reg8Symbol::E),
            0x1E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Byte),
            0x1F => Instruction::RRA,

            0x20 => Instruction::JR(Condition::NZ),
            0x21 => Instruction::LD_16_16(Op16::Reg(Reg16Symbol::HL), Op16::Bytes),
            0x22 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HLI), Op8::Reg(Reg8Symbol::A)),
            0x23 => Instruction::INC_r16(Reg16Symbol::HL),
            0x24 => Instruction::INC_r8(Reg8Symbol::H),
            0x25 => Instruction::DEC_r8(Reg8Symbol::H),
            0x26 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Byte),
            0x27 => Instruction::DAA,
            0x28 => Instruction::JR(Condition::Z),
            0x29 => Instruction::ADD_HL_r16(Reg16Symbol::HL),
            0x2A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Addr(Reg16Symbol::HLI)),
            0x2B => Instruction::DEC_r16(Reg16Symbol::HL),
            0x2C => Instruction::INC_r8(Reg8Symbol::L),
            0x2D => Instruction::DEC_r8(Reg8Symbol::L),
            0x2E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Byte),
            0x2F => Instruction::CPL,

            0x30 => Instruction::JR(Condition::NC),
            0x31 => Instruction::LD_16_16(Op16::Reg(Reg16Symbol::SP), Op16::Bytes),
            0x32 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HLD), Op8::Reg(Reg8Symbol::A)),
            0x33 => Instruction::INC_r16(Reg16Symbol::SP),
            0x34 => Instruction::INC_r16(Reg16Symbol::HL),
            0x35 => Instruction::DEC_r16(Reg16Symbol::HL),
            0x36 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Byte),
            0x37 => Instruction::SCF,
            0x38 => Instruction::JR(Condition::C),
            0x39 => Instruction::ADD_HL_r16(Reg16Symbol::SP),
            0x3A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Addr(Reg16Symbol::HLD)),
            0x3B => Instruction::DEC_r16(Reg16Symbol::SP),
            0x3C => Instruction::INC_r8(Reg8Symbol::A),
            0x3D => Instruction::DEC_r8(Reg8Symbol::A),
            0x3E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Byte),
            0x3F => Instruction::CCF,

            0x40 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Reg(Reg8Symbol::B)),
            0x41 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Reg(Reg8Symbol::C)),
            0x42 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Reg(Reg8Symbol::D)),
            0x43 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Reg(Reg8Symbol::E)),
            0x44 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Reg(Reg8Symbol::H)),
            0x45 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Reg(Reg8Symbol::L)),
            0x46 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Addr(Reg16Symbol::HL)),
            0x47 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::B), Op8::Reg(Reg8Symbol::A)),
            0x48 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Reg(Reg8Symbol::B)),
            0x49 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Reg(Reg8Symbol::C)),
            0x4A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Reg(Reg8Symbol::D)),
            0x4B => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Reg(Reg8Symbol::E)),
            0x4C => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Reg(Reg8Symbol::H)),
            0x4D => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Reg(Reg8Symbol::L)),
            0x4E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Addr(Reg16Symbol::HL)),
            0x4F => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::C), Op8::Reg(Reg8Symbol::A)),

            0x50 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Reg(Reg8Symbol::B)),
            0x51 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Reg(Reg8Symbol::C)),
            0x52 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Reg(Reg8Symbol::D)),
            0x53 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Reg(Reg8Symbol::E)),
            0x54 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Reg(Reg8Symbol::H)),
            0x55 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Reg(Reg8Symbol::L)),
            0x56 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Addr(Reg16Symbol::HL)),
            0x57 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::D), Op8::Reg(Reg8Symbol::A)),
            0x58 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Reg(Reg8Symbol::B)),
            0x59 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Reg(Reg8Symbol::C)),
            0x5A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Reg(Reg8Symbol::D)),
            0x5B => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Reg(Reg8Symbol::E)),
            0x5C => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Reg(Reg8Symbol::H)),
            0x5D => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Reg(Reg8Symbol::L)),
            0x5E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Addr(Reg16Symbol::HL)),
            0x5F => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::E), Op8::Reg(Reg8Symbol::A)),

            0x60 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Reg(Reg8Symbol::B)),
            0x61 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Reg(Reg8Symbol::C)),
            0x62 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Reg(Reg8Symbol::D)),
            0x63 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Reg(Reg8Symbol::E)),
            0x64 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Reg(Reg8Symbol::H)),
            0x65 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Reg(Reg8Symbol::L)),
            0x66 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Addr(Reg16Symbol::HL)),
            0x67 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::H), Op8::Reg(Reg8Symbol::A)),
            0x68 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Reg(Reg8Symbol::B)),
            0x69 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Reg(Reg8Symbol::C)),
            0x6A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Reg(Reg8Symbol::D)),
            0x6B => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Reg(Reg8Symbol::E)),
            0x6C => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Reg(Reg8Symbol::H)),
            0x6D => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Reg(Reg8Symbol::L)),
            0x6E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Addr(Reg16Symbol::HL)),
            0x6F => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::L), Op8::Reg(Reg8Symbol::A)),

            0x70 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Reg(Reg8Symbol::B)),
            0x71 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Reg(Reg8Symbol::C)),
            0x72 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Reg(Reg8Symbol::D)),
            0x73 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Reg(Reg8Symbol::E)),
            0x74 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Reg(Reg8Symbol::H)),
            0x75 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Reg(Reg8Symbol::L)),
            0x76 => Instruction::HALT,
            0x77 => Instruction::LD_8_8(Op8::Addr(Reg16Symbol::HL), Op8::Reg(Reg8Symbol::A)),
            0x78 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Reg(Reg8Symbol::B)),
            0x79 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Reg(Reg8Symbol::C)),
            0x7A => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Reg(Reg8Symbol::D)),
            0x7B => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Reg(Reg8Symbol::E)),
            0x7C => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Reg(Reg8Symbol::H)),
            0x7D => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Reg(Reg8Symbol::L)),
            0x7E => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Addr(Reg16Symbol::HL)),
            0x7F => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::Reg(Reg8Symbol::A)),

            0x80 => Instruction::ADD_A(Op8::Reg(Reg8Symbol::B)),
            0x81 => Instruction::ADD_A(Op8::Reg(Reg8Symbol::C)),
            0x82 => Instruction::ADD_A(Op8::Reg(Reg8Symbol::D)),
            0x83 => Instruction::ADD_A(Op8::Reg(Reg8Symbol::E)),
            0x84 => Instruction::ADD_A(Op8::Reg(Reg8Symbol::H)),
            0x85 => Instruction::ADD_A(Op8::Reg(Reg8Symbol::L)),
            0x86 => Instruction::ADD_A(Op8::Addr(Reg16Symbol::HL)),
            0x87 => Instruction::ADD_A(Op8::Reg(Reg8Symbol::A)),
            0x88 => Instruction::ADC_A(Op8::Reg(Reg8Symbol::B)),
            0x89 => Instruction::ADC_A(Op8::Reg(Reg8Symbol::C)),
            0x8A => Instruction::ADC_A(Op8::Reg(Reg8Symbol::D)),
            0x8B => Instruction::ADC_A(Op8::Reg(Reg8Symbol::E)),
            0x8C => Instruction::ADC_A(Op8::Reg(Reg8Symbol::H)),
            0x8D => Instruction::ADC_A(Op8::Reg(Reg8Symbol::L)),
            0x8E => Instruction::ADC_A(Op8::Addr(Reg16Symbol::HL)),
            0x8F => Instruction::ADC_A(Op8::Reg(Reg8Symbol::A)),

            0x90 => Instruction::SUB_A(Op8::Reg(Reg8Symbol::B)),
            0x91 => Instruction::SUB_A(Op8::Reg(Reg8Symbol::C)),
            0x92 => Instruction::SUB_A(Op8::Reg(Reg8Symbol::D)),
            0x93 => Instruction::SUB_A(Op8::Reg(Reg8Symbol::E)),
            0x94 => Instruction::SUB_A(Op8::Reg(Reg8Symbol::H)),
            0x95 => Instruction::SUB_A(Op8::Reg(Reg8Symbol::L)),
            0x96 => Instruction::SUB_A(Op8::Addr(Reg16Symbol::HL)),
            0x97 => Instruction::SUB_A(Op8::Reg(Reg8Symbol::A)),
            0x98 => Instruction::SBC_A(Op8::Reg(Reg8Symbol::B)),
            0x99 => Instruction::SBC_A(Op8::Reg(Reg8Symbol::C)),
            0x9A => Instruction::SBC_A(Op8::Reg(Reg8Symbol::D)),
            0x9B => Instruction::SBC_A(Op8::Reg(Reg8Symbol::E)),
            0x9C => Instruction::SBC_A(Op8::Reg(Reg8Symbol::H)),
            0x9D => Instruction::SBC_A(Op8::Reg(Reg8Symbol::L)),
            0x9E => Instruction::SBC_A(Op8::Addr(Reg16Symbol::HL)),
            0x9F => Instruction::SBC_A(Op8::Reg(Reg8Symbol::A)),

            0xA0 => Instruction::AND_A(Op8::Reg(Reg8Symbol::B)),
            0xA1 => Instruction::AND_A(Op8::Reg(Reg8Symbol::C)),
            0xA2 => Instruction::AND_A(Op8::Reg(Reg8Symbol::D)),
            0xA3 => Instruction::AND_A(Op8::Reg(Reg8Symbol::E)),
            0xA4 => Instruction::AND_A(Op8::Reg(Reg8Symbol::H)),
            0xA5 => Instruction::AND_A(Op8::Reg(Reg8Symbol::L)),
            0xA6 => Instruction::AND_A(Op8::Addr(Reg16Symbol::HL)),
            0xA7 => Instruction::AND_A(Op8::Reg(Reg8Symbol::A)),
            0xA8 => Instruction::XOR_A(Op8::Reg(Reg8Symbol::B)),
            0xA9 => Instruction::XOR_A(Op8::Reg(Reg8Symbol::C)),
            0xAA => Instruction::XOR_A(Op8::Reg(Reg8Symbol::D)),
            0xAB => Instruction::XOR_A(Op8::Reg(Reg8Symbol::E)),
            0xAC => Instruction::XOR_A(Op8::Reg(Reg8Symbol::H)),
            0xAD => Instruction::XOR_A(Op8::Reg(Reg8Symbol::L)),
            0xAE => Instruction::XOR_A(Op8::Addr(Reg16Symbol::HL)),
            0xAF => Instruction::XOR_A(Op8::Reg(Reg8Symbol::A)),

            0xB0 => Instruction::OR_A(Op8::Reg(Reg8Symbol::B)),
            0xB1 => Instruction::OR_A(Op8::Reg(Reg8Symbol::C)),
            0xB2 => Instruction::OR_A(Op8::Reg(Reg8Symbol::D)),
            0xB3 => Instruction::OR_A(Op8::Reg(Reg8Symbol::E)),
            0xB4 => Instruction::OR_A(Op8::Reg(Reg8Symbol::H)),
            0xB5 => Instruction::OR_A(Op8::Reg(Reg8Symbol::L)),
            0xB6 => Instruction::OR_A(Op8::Addr(Reg16Symbol::HL)),
            0xB7 => Instruction::OR_A(Op8::Reg(Reg8Symbol::A)),
            0xB8 => Instruction::CP_A(Op8::Reg(Reg8Symbol::B)),
            0xB9 => Instruction::CP_A(Op8::Reg(Reg8Symbol::C)),
            0xBA => Instruction::CP_A(Op8::Reg(Reg8Symbol::D)),
            0xBB => Instruction::CP_A(Op8::Reg(Reg8Symbol::E)),
            0xBC => Instruction::CP_A(Op8::Reg(Reg8Symbol::H)),
            0xBD => Instruction::CP_A(Op8::Reg(Reg8Symbol::L)),
            0xBE => Instruction::CP_A(Op8::Addr(Reg16Symbol::HL)),
            0xBF => Instruction::CP_A(Op8::Reg(Reg8Symbol::A)),

            0xC0 => Instruction::RET(Condition::NZ),
            0xC1 => Instruction::POP_r16(Reg16Symbol::BC),
            0xC2 => Instruction::JP(Condition::NZ, Op16::Bytes),
            0xC3 => Instruction::JP(Condition::None, Op16::Bytes),
            0xC4 => Instruction::CALL(Condition::NZ),
            0xC5 => Instruction::PUSH_r16(Reg16Symbol::BC),
            0xC6 => Instruction::ADD_A(Op8::Byte),
            0xC7 => Instruction::RST(0x00),
            0xC8 => Instruction::RET(Condition::Z),
            0xC9 => Instruction::RET(Condition::None),
            0xCA => Instruction::JP(Condition::Z, Op16::Bytes),
            0xCB => Instruction::PREFIX,
            0xCC => Instruction::CALL(Condition::Z),
            0xCD => Instruction::CALL(Condition::None),
            0xCE => Instruction::ADC_A(Op8::Byte),
            0xCF => Instruction::RST(0x08),

            0xD0 => Instruction::RET(Condition::NC),
            0xD1 => Instruction::POP_r16(Reg16Symbol::DE),
            0xD2 => Instruction::JP(Condition::NC, Op16::Bytes),
            0xD4 => Instruction::CALL(Condition::NC),
            0xD5 => Instruction::PUSH_r16(Reg16Symbol::DE),
            0xD6 => Instruction::SUB_A(Op8::Byte),
            0xD7 => Instruction::RST(0x10),
            0xD8 => Instruction::RET(Condition::C),
            0xD9 => Instruction::RETI,
            0xDA => Instruction::JP(Condition::C, Op16::Bytes),
            0xDC => Instruction::CALL(Condition::C),
            0xDE => Instruction::SBC_A(Op8::Byte),
            0xDF => Instruction::RST(0x18),

            0xE0 => Instruction::LD_8_8(Op8::HighByte, Op8::Reg(Reg8Symbol::A)),
            0xE1 => Instruction::POP_r16(Reg16Symbol::HL),
            0xE2 => Instruction::LD_8_8(Op8::C, Op8::Reg(Reg8Symbol::A)),
            0xE5 => Instruction::PUSH_r16(Reg16Symbol::HL),
            0xE6 => Instruction::AND_A(Op8::Byte),
            0xE7 => Instruction::RST(0x20),
            0xE8 => Instruction::ADD_SP_e8,
            0xE9 => Instruction::JP(Condition::None, Op16::Reg(Reg16Symbol::HL)),
            0xEA => Instruction::LD_8_8(Op8::AddrBytes, Op8::Reg(Reg8Symbol::A)),
            0xEE => Instruction::XOR_A(Op8::Byte),
            0xEF => Instruction::RST(0x28),

            0xF0 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::HighByte),
            0xF1 => Instruction::POP_AF,
            0xF2 => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::C),
            0xF3 => Instruction::DI,
            0xF5 => Instruction::PUSH_AF,
            0xF6 => Instruction::OR_A(Op8::Byte),
            0xF7 => Instruction::RST(0x30),
            0xF8 => Instruction::LD_HL_SP_e8,
            0xF9 => Instruction::LD_16_16(Op16::Reg(Reg16Symbol::SP), Op16::Reg(Reg16Symbol::HL)),
            0xFA => Instruction::LD_8_8(Op8::Reg(Reg8Symbol::A), Op8::AddrBytes),
            0xFB => Instruction::EI,
            0xFE => Instruction::CP_A(Op8::Byte),
            0xFF => Instruction::RST(0x38),

            _ => {
                println!("Warning: no implementation for opcode {:X}", opcode);
                Instruction::NotImplemented
            }
        }
    }

    pub fn decode_prefix(opcode: u8) -> Instruction {
        match opcode {
            0x00 => Instruction::RL(Op8::Reg(Reg8Symbol::B), true),
            0x01 => Instruction::RL(Op8::Reg(Reg8Symbol::C), true),
            0x02 => Instruction::RL(Op8::Reg(Reg8Symbol::D), true),
            0x03 => Instruction::RL(Op8::Reg(Reg8Symbol::E), true),
            0x04 => Instruction::RL(Op8::Reg(Reg8Symbol::H), true),
            0x05 => Instruction::RL(Op8::Reg(Reg8Symbol::L), true),
            0x06 => Instruction::RL(Op8::Addr(Reg16Symbol::HL), true),
            0x07 => Instruction::RL(Op8::Reg(Reg8Symbol::A), true),

            0x08 => Instruction::RR(Op8::Reg(Reg8Symbol::B), true),
            0x09 => Instruction::RR(Op8::Reg(Reg8Symbol::C), true),
            0x0A => Instruction::RR(Op8::Reg(Reg8Symbol::D), true),
            0x0B => Instruction::RR(Op8::Reg(Reg8Symbol::E), true),
            0x0C => Instruction::RR(Op8::Reg(Reg8Symbol::H), true),
            0x0D => Instruction::RR(Op8::Reg(Reg8Symbol::L), true),
            0x0E => Instruction::RR(Op8::Addr(Reg16Symbol::HL), true),
            0x0F => Instruction::RR(Op8::Reg(Reg8Symbol::A), true),

            0x10 => Instruction::RL(Op8::Reg(Reg8Symbol::B), false),
            0x11 => Instruction::RL(Op8::Reg(Reg8Symbol::C), false),
            0x12 => Instruction::RL(Op8::Reg(Reg8Symbol::D), false),
            0x13 => Instruction::RL(Op8::Reg(Reg8Symbol::E), false),
            0x14 => Instruction::RL(Op8::Reg(Reg8Symbol::H), false),
            0x15 => Instruction::RL(Op8::Reg(Reg8Symbol::L), false),
            0x16 => Instruction::RL(Op8::Addr(Reg16Symbol::HL), false),
            0x17 => Instruction::RL(Op8::Reg(Reg8Symbol::A), false),

            0x18 => Instruction::RR(Op8::Reg(Reg8Symbol::B), false),
            0x19 => Instruction::RR(Op8::Reg(Reg8Symbol::C), false),
            0x1A => Instruction::RR(Op8::Reg(Reg8Symbol::D), false),
            0x1B => Instruction::RR(Op8::Reg(Reg8Symbol::E), false),
            0x1C => Instruction::RR(Op8::Reg(Reg8Symbol::H), false),
            0x1D => Instruction::RR(Op8::Reg(Reg8Symbol::L), false),
            0x1E => Instruction::RR(Op8::Addr(Reg16Symbol::HL), false),
            0x1F => Instruction::RR(Op8::Reg(Reg8Symbol::A), false),

            0x20 => Instruction::SLA(Op8::Reg(Reg8Symbol::B)),
            0x21 => Instruction::SLA(Op8::Reg(Reg8Symbol::C)),
            0x22 => Instruction::SLA(Op8::Reg(Reg8Symbol::D)),
            0x23 => Instruction::SLA(Op8::Reg(Reg8Symbol::E)),
            0x24 => Instruction::SLA(Op8::Reg(Reg8Symbol::H)),
            0x25 => Instruction::SLA(Op8::Reg(Reg8Symbol::L)),
            0x26 => Instruction::SLA(Op8::Addr(Reg16Symbol::HL)),
            0x27 => Instruction::SLA(Op8::Reg(Reg8Symbol::A)),

            0x28 => Instruction::SRA(Op8::Reg(Reg8Symbol::B)),
            0x29 => Instruction::SRA(Op8::Reg(Reg8Symbol::C)),
            0x2A => Instruction::SRA(Op8::Reg(Reg8Symbol::D)),
            0x2B => Instruction::SRA(Op8::Reg(Reg8Symbol::E)),
            0x2C => Instruction::SRA(Op8::Reg(Reg8Symbol::H)),
            0x2D => Instruction::SRA(Op8::Reg(Reg8Symbol::L)),
            0x2E => Instruction::SRA(Op8::Addr(Reg16Symbol::HL)),
            0x2F => Instruction::SRA(Op8::Reg(Reg8Symbol::A)),

            0x30 => Instruction::SWAP(Op8::Reg(Reg8Symbol::B)),
            0x31 => Instruction::SWAP(Op8::Reg(Reg8Symbol::C)),
            0x32 => Instruction::SWAP(Op8::Reg(Reg8Symbol::D)),
            0x33 => Instruction::SWAP(Op8::Reg(Reg8Symbol::E)),
            0x34 => Instruction::SWAP(Op8::Reg(Reg8Symbol::H)),
            0x35 => Instruction::SWAP(Op8::Reg(Reg8Symbol::L)),
            0x36 => Instruction::SWAP(Op8::Addr(Reg16Symbol::HL)),
            0x37 => Instruction::SWAP(Op8::Reg(Reg8Symbol::A)),

            0x38 => Instruction::SRL(Op8::Reg(Reg8Symbol::B)),
            0x39 => Instruction::SRL(Op8::Reg(Reg8Symbol::C)),
            0x3A => Instruction::SRL(Op8::Reg(Reg8Symbol::D)),
            0x3B => Instruction::SRL(Op8::Reg(Reg8Symbol::E)),
            0x3C => Instruction::SRL(Op8::Reg(Reg8Symbol::H)),
            0x3D => Instruction::SRL(Op8::Reg(Reg8Symbol::L)),
            0x3E => Instruction::SRL(Op8::Addr(Reg16Symbol::HL)),
            0x3F => Instruction::SRL(Op8::Reg(Reg8Symbol::A)),

            0x40 => Instruction::BIT(0, Op8::Reg(Reg8Symbol::B)),
            0x41 => Instruction::BIT(0, Op8::Reg(Reg8Symbol::C)),
            0x42 => Instruction::BIT(0, Op8::Reg(Reg8Symbol::D)),
            0x43 => Instruction::BIT(0, Op8::Reg(Reg8Symbol::E)),
            0x44 => Instruction::BIT(0, Op8::Reg(Reg8Symbol::H)),
            0x45 => Instruction::BIT(0, Op8::Reg(Reg8Symbol::L)),
            0x46 => Instruction::BIT(0, Op8::Addr(Reg16Symbol::HL)),
            0x47 => Instruction::BIT(0, Op8::Reg(Reg8Symbol::A)),

            0x48 => Instruction::BIT(1, Op8::Reg(Reg8Symbol::B)),
            0x49 => Instruction::BIT(1, Op8::Reg(Reg8Symbol::C)),
            0x4A => Instruction::BIT(1, Op8::Reg(Reg8Symbol::D)),
            0x4B => Instruction::BIT(1, Op8::Reg(Reg8Symbol::E)),
            0x4C => Instruction::BIT(1, Op8::Reg(Reg8Symbol::H)),
            0x4D => Instruction::BIT(1, Op8::Reg(Reg8Symbol::L)),
            0x4E => Instruction::BIT(1, Op8::Addr(Reg16Symbol::HL)),
            0x4F => Instruction::BIT(1, Op8::Reg(Reg8Symbol::A)),

            0x50 => Instruction::BIT(2, Op8::Reg(Reg8Symbol::B)),
            0x51 => Instruction::BIT(2, Op8::Reg(Reg8Symbol::C)),
            0x52 => Instruction::BIT(2, Op8::Reg(Reg8Symbol::D)),
            0x53 => Instruction::BIT(2, Op8::Reg(Reg8Symbol::E)),
            0x54 => Instruction::BIT(2, Op8::Reg(Reg8Symbol::H)),
            0x55 => Instruction::BIT(2, Op8::Reg(Reg8Symbol::L)),
            0x56 => Instruction::BIT(2, Op8::Addr(Reg16Symbol::HL)),
            0x57 => Instruction::BIT(2, Op8::Reg(Reg8Symbol::A)),

            0x58 => Instruction::BIT(3, Op8::Reg(Reg8Symbol::B)),
            0x59 => Instruction::BIT(3, Op8::Reg(Reg8Symbol::C)),
            0x5A => Instruction::BIT(3, Op8::Reg(Reg8Symbol::D)),
            0x5B => Instruction::BIT(3, Op8::Reg(Reg8Symbol::E)),
            0x5C => Instruction::BIT(3, Op8::Reg(Reg8Symbol::H)),
            0x5D => Instruction::BIT(3, Op8::Reg(Reg8Symbol::L)),
            0x5E => Instruction::BIT(3, Op8::Addr(Reg16Symbol::HL)),
            0x5F => Instruction::BIT(3, Op8::Reg(Reg8Symbol::A)),

            0x60 => Instruction::BIT(4, Op8::Reg(Reg8Symbol::B)),
            0x61 => Instruction::BIT(4, Op8::Reg(Reg8Symbol::C)),
            0x62 => Instruction::BIT(4, Op8::Reg(Reg8Symbol::D)),
            0x63 => Instruction::BIT(4, Op8::Reg(Reg8Symbol::E)),
            0x64 => Instruction::BIT(4, Op8::Reg(Reg8Symbol::H)),
            0x65 => Instruction::BIT(4, Op8::Reg(Reg8Symbol::L)),
            0x66 => Instruction::BIT(4, Op8::Addr(Reg16Symbol::HL)),
            0x67 => Instruction::BIT(4, Op8::Reg(Reg8Symbol::A)),

            0x68 => Instruction::BIT(5, Op8::Reg(Reg8Symbol::B)),
            0x69 => Instruction::BIT(5, Op8::Reg(Reg8Symbol::C)),
            0x6A => Instruction::BIT(5, Op8::Reg(Reg8Symbol::D)),
            0x6B => Instruction::BIT(5, Op8::Reg(Reg8Symbol::E)),
            0x6C => Instruction::BIT(5, Op8::Reg(Reg8Symbol::H)),
            0x6D => Instruction::BIT(5, Op8::Reg(Reg8Symbol::L)),
            0x6E => Instruction::BIT(5, Op8::Addr(Reg16Symbol::HL)),
            0x6F => Instruction::BIT(5, Op8::Reg(Reg8Symbol::A)),

            0x70 => Instruction::BIT(6, Op8::Reg(Reg8Symbol::B)),
            0x71 => Instruction::BIT(6, Op8::Reg(Reg8Symbol::C)),
            0x72 => Instruction::BIT(6, Op8::Reg(Reg8Symbol::D)),
            0x73 => Instruction::BIT(6, Op8::Reg(Reg8Symbol::E)),
            0x74 => Instruction::BIT(6, Op8::Reg(Reg8Symbol::H)),
            0x75 => Instruction::BIT(6, Op8::Reg(Reg8Symbol::L)),
            0x76 => Instruction::BIT(6, Op8::Addr(Reg16Symbol::HL)),
            0x77 => Instruction::BIT(6, Op8::Reg(Reg8Symbol::A)),

            0x78 => Instruction::BIT(7, Op8::Reg(Reg8Symbol::B)),
            0x79 => Instruction::BIT(7, Op8::Reg(Reg8Symbol::C)),
            0x7A => Instruction::BIT(7, Op8::Reg(Reg8Symbol::D)),
            0x7B => Instruction::BIT(7, Op8::Reg(Reg8Symbol::E)),
            0x7C => Instruction::BIT(7, Op8::Reg(Reg8Symbol::H)),
            0x7D => Instruction::BIT(7, Op8::Reg(Reg8Symbol::L)),
            0x7E => Instruction::BIT(7, Op8::Addr(Reg16Symbol::HL)),
            0x7F => Instruction::BIT(7, Op8::Reg(Reg8Symbol::A)),

            0x80 => Instruction::RES(0, Op8::Reg(Reg8Symbol::B)),
            0x81 => Instruction::RES(0, Op8::Reg(Reg8Symbol::C)),
            0x82 => Instruction::RES(0, Op8::Reg(Reg8Symbol::D)),
            0x83 => Instruction::RES(0, Op8::Reg(Reg8Symbol::E)),
            0x84 => Instruction::RES(0, Op8::Reg(Reg8Symbol::H)),
            0x85 => Instruction::RES(0, Op8::Reg(Reg8Symbol::L)),
            0x86 => Instruction::RES(0, Op8::Addr(Reg16Symbol::HL)),
            0x87 => Instruction::RES(0, Op8::Reg(Reg8Symbol::A)),

            0x88 => Instruction::RES(1, Op8::Reg(Reg8Symbol::B)),
            0x89 => Instruction::RES(1, Op8::Reg(Reg8Symbol::C)),
            0x8A => Instruction::RES(1, Op8::Reg(Reg8Symbol::D)),
            0x8B => Instruction::RES(1, Op8::Reg(Reg8Symbol::E)),
            0x8C => Instruction::RES(1, Op8::Reg(Reg8Symbol::H)),
            0x8D => Instruction::RES(1, Op8::Reg(Reg8Symbol::L)),
            0x8E => Instruction::RES(1, Op8::Addr(Reg16Symbol::HL)),
            0x8F => Instruction::RES(1, Op8::Reg(Reg8Symbol::A)),

            0x90 => Instruction::RES(2, Op8::Reg(Reg8Symbol::B)),
            0x91 => Instruction::RES(2, Op8::Reg(Reg8Symbol::C)),
            0x92 => Instruction::RES(2, Op8::Reg(Reg8Symbol::D)),
            0x93 => Instruction::RES(2, Op8::Reg(Reg8Symbol::E)),
            0x94 => Instruction::RES(2, Op8::Reg(Reg8Symbol::H)),
            0x95 => Instruction::RES(2, Op8::Reg(Reg8Symbol::L)),
            0x96 => Instruction::RES(2, Op8::Addr(Reg16Symbol::HL)),
            0x97 => Instruction::RES(2, Op8::Reg(Reg8Symbol::A)),

            0x98 => Instruction::RES(3, Op8::Reg(Reg8Symbol::B)),
            0x99 => Instruction::RES(3, Op8::Reg(Reg8Symbol::C)),
            0x9A => Instruction::RES(3, Op8::Reg(Reg8Symbol::D)),
            0x9B => Instruction::RES(3, Op8::Reg(Reg8Symbol::E)),
            0x9C => Instruction::RES(3, Op8::Reg(Reg8Symbol::H)),
            0x9D => Instruction::RES(3, Op8::Reg(Reg8Symbol::L)),
            0x9E => Instruction::RES(3, Op8::Addr(Reg16Symbol::HL)),
            0x9F => Instruction::RES(3, Op8::Reg(Reg8Symbol::A)),

            0xA0 => Instruction::RES(4, Op8::Reg(Reg8Symbol::B)),
            0xA1 => Instruction::RES(4, Op8::Reg(Reg8Symbol::C)),
            0xA2 => Instruction::RES(4, Op8::Reg(Reg8Symbol::D)),
            0xA3 => Instruction::RES(4, Op8::Reg(Reg8Symbol::E)),
            0xA4 => Instruction::RES(4, Op8::Reg(Reg8Symbol::H)),
            0xA5 => Instruction::RES(4, Op8::Reg(Reg8Symbol::L)),
            0xA6 => Instruction::RES(4, Op8::Addr(Reg16Symbol::HL)),
            0xA7 => Instruction::RES(4, Op8::Reg(Reg8Symbol::A)),

            0xA8 => Instruction::RES(5, Op8::Reg(Reg8Symbol::B)),
            0xA9 => Instruction::RES(5, Op8::Reg(Reg8Symbol::C)),
            0xAA => Instruction::RES(5, Op8::Reg(Reg8Symbol::D)),
            0xAB => Instruction::RES(5, Op8::Reg(Reg8Symbol::E)),
            0xAC => Instruction::RES(5, Op8::Reg(Reg8Symbol::H)),
            0xAD => Instruction::RES(5, Op8::Reg(Reg8Symbol::L)),
            0xAE => Instruction::RES(5, Op8::Addr(Reg16Symbol::HL)),
            0xAF => Instruction::RES(5, Op8::Reg(Reg8Symbol::A)),

            0xB0 => Instruction::RES(6, Op8::Reg(Reg8Symbol::B)),
            0xB1 => Instruction::RES(6, Op8::Reg(Reg8Symbol::C)),
            0xB2 => Instruction::RES(6, Op8::Reg(Reg8Symbol::D)),
            0xB3 => Instruction::RES(6, Op8::Reg(Reg8Symbol::E)),
            0xB4 => Instruction::RES(6, Op8::Reg(Reg8Symbol::H)),
            0xB5 => Instruction::RES(6, Op8::Reg(Reg8Symbol::L)),
            0xB6 => Instruction::RES(6, Op8::Addr(Reg16Symbol::HL)),
            0xB7 => Instruction::RES(6, Op8::Reg(Reg8Symbol::A)),

            0xB8 => Instruction::RES(7, Op8::Reg(Reg8Symbol::B)),
            0xB9 => Instruction::RES(7, Op8::Reg(Reg8Symbol::C)),
            0xBA => Instruction::RES(7, Op8::Reg(Reg8Symbol::D)),
            0xBB => Instruction::RES(7, Op8::Reg(Reg8Symbol::E)),
            0xBC => Instruction::RES(7, Op8::Reg(Reg8Symbol::H)),
            0xBD => Instruction::RES(7, Op8::Reg(Reg8Symbol::L)),
            0xBE => Instruction::RES(7, Op8::Addr(Reg16Symbol::HL)),
            0xBF => Instruction::RES(7, Op8::Reg(Reg8Symbol::A)),

            0xC0 => Instruction::SET(0, Op8::Reg(Reg8Symbol::B)),
            0xC1 => Instruction::SET(0, Op8::Reg(Reg8Symbol::C)),
            0xC2 => Instruction::SET(0, Op8::Reg(Reg8Symbol::D)),
            0xC3 => Instruction::SET(0, Op8::Reg(Reg8Symbol::E)),
            0xC4 => Instruction::SET(0, Op8::Reg(Reg8Symbol::H)),
            0xC5 => Instruction::SET(0, Op8::Reg(Reg8Symbol::L)),
            0xC6 => Instruction::SET(0, Op8::Addr(Reg16Symbol::HL)),
            0xC7 => Instruction::SET(0, Op8::Reg(Reg8Symbol::A)),

            0xC8 => Instruction::SET(1, Op8::Reg(Reg8Symbol::B)),
            0xC9 => Instruction::SET(1, Op8::Reg(Reg8Symbol::C)),
            0xCA => Instruction::SET(1, Op8::Reg(Reg8Symbol::D)),
            0xCB => Instruction::SET(1, Op8::Reg(Reg8Symbol::E)),
            0xCC => Instruction::SET(1, Op8::Reg(Reg8Symbol::H)),
            0xCD => Instruction::SET(1, Op8::Reg(Reg8Symbol::L)),
            0xCE => Instruction::SET(1, Op8::Addr(Reg16Symbol::HL)),
            0xCF => Instruction::SET(1, Op8::Reg(Reg8Symbol::A)),

            0xD0 => Instruction::SET(2, Op8::Reg(Reg8Symbol::B)),
            0xD1 => Instruction::SET(2, Op8::Reg(Reg8Symbol::C)),
            0xD2 => Instruction::SET(2, Op8::Reg(Reg8Symbol::D)),
            0xD3 => Instruction::SET(2, Op8::Reg(Reg8Symbol::E)),
            0xD4 => Instruction::SET(2, Op8::Reg(Reg8Symbol::H)),
            0xD5 => Instruction::SET(2, Op8::Reg(Reg8Symbol::L)),
            0xD6 => Instruction::SET(2, Op8::Addr(Reg16Symbol::HL)),
            0xD7 => Instruction::SET(2, Op8::Reg(Reg8Symbol::A)),

            0xD8 => Instruction::SET(3, Op8::Reg(Reg8Symbol::B)),
            0xD9 => Instruction::SET(3, Op8::Reg(Reg8Symbol::C)),
            0xDA => Instruction::SET(3, Op8::Reg(Reg8Symbol::D)),
            0xDB => Instruction::SET(3, Op8::Reg(Reg8Symbol::E)),
            0xDC => Instruction::SET(3, Op8::Reg(Reg8Symbol::H)),
            0xDD => Instruction::SET(3, Op8::Reg(Reg8Symbol::L)),
            0xDE => Instruction::SET(3, Op8::Addr(Reg16Symbol::HL)),
            0xDF => Instruction::SET(3, Op8::Reg(Reg8Symbol::A)),

            0xE0 => Instruction::SET(4, Op8::Reg(Reg8Symbol::B)),
            0xE1 => Instruction::SET(4, Op8::Reg(Reg8Symbol::C)),
            0xE2 => Instruction::SET(4, Op8::Reg(Reg8Symbol::D)),
            0xE3 => Instruction::SET(4, Op8::Reg(Reg8Symbol::E)),
            0xE4 => Instruction::SET(4, Op8::Reg(Reg8Symbol::H)),
            0xE5 => Instruction::SET(4, Op8::Reg(Reg8Symbol::L)),
            0xE6 => Instruction::SET(4, Op8::Addr(Reg16Symbol::HL)),
            0xE7 => Instruction::SET(4, Op8::Reg(Reg8Symbol::A)),

            0xE8 => Instruction::SET(5, Op8::Reg(Reg8Symbol::B)),
            0xE9 => Instruction::SET(5, Op8::Reg(Reg8Symbol::C)),
            0xEA => Instruction::SET(5, Op8::Reg(Reg8Symbol::D)),
            0xEB => Instruction::SET(5, Op8::Reg(Reg8Symbol::E)),
            0xEC => Instruction::SET(5, Op8::Reg(Reg8Symbol::H)),
            0xED => Instruction::SET(5, Op8::Reg(Reg8Symbol::L)),
            0xEE => Instruction::SET(5, Op8::Addr(Reg16Symbol::HL)),
            0xEF => Instruction::SET(5, Op8::Reg(Reg8Symbol::A)),

            0xF0 => Instruction::SET(6, Op8::Reg(Reg8Symbol::B)),
            0xF1 => Instruction::SET(6, Op8::Reg(Reg8Symbol::C)),
            0xF2 => Instruction::SET(6, Op8::Reg(Reg8Symbol::D)),
            0xF3 => Instruction::SET(6, Op8::Reg(Reg8Symbol::E)),
            0xF4 => Instruction::SET(6, Op8::Reg(Reg8Symbol::H)),
            0xF5 => Instruction::SET(6, Op8::Reg(Reg8Symbol::L)),
            0xF6 => Instruction::SET(6, Op8::Addr(Reg16Symbol::HL)),
            0xF7 => Instruction::SET(6, Op8::Reg(Reg8Symbol::A)),

            0xF8 => Instruction::SET(7, Op8::Reg(Reg8Symbol::B)),
            0xF9 => Instruction::SET(7, Op8::Reg(Reg8Symbol::C)),
            0xFA => Instruction::SET(7, Op8::Reg(Reg8Symbol::D)),
            0xFB => Instruction::SET(7, Op8::Reg(Reg8Symbol::E)),
            0xFC => Instruction::SET(7, Op8::Reg(Reg8Symbol::H)),
            0xFD => Instruction::SET(7, Op8::Reg(Reg8Symbol::L)),
            0xFE => Instruction::SET(7, Op8::Addr(Reg16Symbol::HL)),
            0xFF => Instruction::SET(7, Op8::Reg(Reg8Symbol::A)),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NOP => write!(f, "NOP"),
            Self::LD_8_8(dst, src) => write!(f, "LD {dst} {src}"),
            Self::LD_16_16(dst, src) => write!(f, "LD {dst} {src}"),
            Self::LD_a16_SP => write!(f, "LD a16 SP"),
            Self::LD_HL_SP_e8 => write!(f, "LD HL SP+e8"),
            Self::INC_r16(r) => write!(f, "INC {:?}", r),
            Self::INC_r8(r) => write!(f, "INC {:?}", r),
            Self::DEC_r16(r) => write!(f, "DEC {:?}", r),
            Self::DEC_r8(r) => write!(f, "DEC {:?}", r),
            Self::ADD_HL_r16(r) => write!(f, "ADD HL {:?}", r),
            Self::ADD_A(op) => write!(f, "ADD A {op}"),
            Self::ADD_SP_e8 => write!(f, "ADD SP e8"),
            Self::ADC_A(op) => write!(f, "ADC A {op}"),
            Self::SUB_A(op) => write!(f, "SUB A {op}"),
            Self::SBC_A(op) => write!(f, "SBC A {op}"),
            Self::AND_A(op) => write!(f, "AND A {op}"),
            Self::XOR_A(op) => write!(f, "XOR A {op}"),
            Self::OR_A(op) => write!(f, "OR A {op}"),
            Self::CP_A(op) => write!(f, "CP A {op}"),
            Self::JP(cond, op) => write!(f, "JP{cond} {op}"),
            Self::JR(cond) => write!(f, "JR{cond}"),
            Self::RET(cond) => write!(f, "RET{cond}"),
            Self::RETI => write!(f, "RETI"),
            Self::PUSH_r16(r) => write!(f, "PUSH {:?}", r),
            Self::POP_r16(r) => write!(f, "POP {:?}", r),
            Self::PUSH_AF => write!(f, "PUSH AF"),
            Self::POP_AF => write!(f, "POP AF"),
            Self::CALL(cond) => write!(f, "CALL{cond}"),
            Self::RST(n) => write!(f, "RST ${:04X}", n),
            Self::RLCA => write!(f, "RLCA"),
            Self::RRCA => write!(f, "RRCA"),
            Self::RLA => write!(f, "RLA"),
            Self::RRA => write!(f, "RRA"),
            Self::DAA => write!(f, "DAA"),
            Self::CPL => write!(f, "CPL"),
            Self::SCF => write!(f, "SCF"),
            Self::CCF => write!(f, "CCF"),
            Self::DI => write!(f, "DI"),
            Self::EI => write!(f, "EI"),
            Self::STOP => write!(f, "STOP"),
            Self::HALT => write!(f, "HALT"),
            Self::PREFIX => write!(f, "PREFIX"),

            Self::RL(op, cir) => {
                let c = if *cir { "C" } else { "" };
                write!(f, "RL{c} {op}")
            }

            Self::RR(op, cir) => {
                let c = if *cir { "C" } else { "" };
                write!(f, "RR{c} {op}")
            }

            Self::SLA(op) => write!(f, "SLA {op}"),
            Self::SRA(op) => write!(f, "SRA {op}"),
            Self::SWAP(op) => write!(f, "SWAP {op}"),
            Self::SRL(op) => write!(f, "SRL {op}"),
            Self::BIT(n, op) => write!(f, "BIT {n} {op}"),
            Self::RES(n, op) => write!(f, "RES {n} {op}"),
            Self::SET(n, op) => write!(f, "SET {n} {op}"),

            Self::NotImplemented => write!(f, "Not Implemented"),
        }
    }
}
