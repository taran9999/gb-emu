#[allow(non_camel_case_types)]
enum Instruction {
    NOP,

    LD_r8_r8(Reg8Symbol, Reg8Symbol),
    LD_r8_n8(Reg8Symbol),
    LD_r8_r16(Reg8Symbol, Reg16Symbol),
    LD_r16_r8(Reg16Symbol, Reg8Symbol),
    LD_r16_n16(Reg16Symbol),
    LD_a16_SP,
    LD_SP_n16,
    LD_SP_HL,
    LD_a16_A,
    LD_A_a16,
    LD_HL_n8,
    LD_HL_SP_e8,
    LDH_n8_A,
    LDH_A_n8,
    LDH_C_A,
    LDH_A_C,

    INC_r16(Reg16Symbol),
    INC_r8(Reg8Symbol),
    INC_SP,

    DEC_r16(Reg16Symbol),
    DEC_r8(Reg8Symbol),
    DEC_SP,

    ADD_HL_r16(Reg16Symbol),
    ADD_HL_SP,
    ADD_A_r8(Reg8Symbol),
    ADD_A_HL,
    ADD_A_n8,
    ADD_SP_e8,

    ADC_A_r8(Reg8Symbol),
    ADC_A_HL,
    ADC_A_n8,

    SUB_A_r8(Reg8Symbol),
    SUB_A_HL,
    SUB_A_n8,

    SBC_A_r8(Reg8Symbol),
    SBC_A_HL,
    SBC_A_n8,

    AND_A_r8(Reg8Symbol),
    AND_A_HL,
    AND_A_n8,

    XOR_A_r8(Reg8Symbol),
    XOR_A_HL,
    XOR_A_n8,

    OR_A_r8(Reg8Symbol),
    OR_A_HL,
    OR_A_n8,

    CP_A_r8(Reg8Symbol),
    CP_A_HL,
    CP_A_n8,

    JP_n16,
    JP_n16_Conditional(FlagSymbol, bool),
    JP_HL,

    JR_n16,
    JR_n16_Conditional(FlagSymbol, bool),

    RET,
    RET_Conditional(FlagSymbol, bool),
    RETI,

    PUSH_r16(Reg16Symbol),
    POP_r16(Reg16Symbol),
    PUSH_AF,
    POP_AF,

    CALL_n16,
    CALL_n16_Conditional(FlagSymbol, bool),
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
    RLC(Reg8Symbol),
    RLC_HL,
    RL(Reg8Symbol),
    RL_HL,
    RRC(Reg8Symbol),
    RRC_HL,
    RR(Reg8Symbol),
    RR_HL,

    SLA(Reg8Symbol),
    SLA_HL,
    SRA(Reg8Symbol),
    SRA_HL,

    SWAP(Reg8Symbol),
    SWAP_HL,

    SRL(Reg8Symbol),
    SRL_HL,

    // use u8 as u3 - values should range from 0 to 7
    BIT(u8, Reg8Symbol),
    BIT_HL(u8),
    RES(u8, Reg8Symbol),
    RES_HL(u8),
    SET(u8, Reg8Symbol),
    SET_HL(u8),

    NotImplemented,
}
