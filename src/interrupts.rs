pub enum Interrupt {
    VBlank,
    LCD,
    Timer,
    Serial,
    Joypad,
}

impl Interrupt {
    pub fn bit_enable(&self) -> u8 {
        match self {
            Interrupt::VBlank => 1,
            Interrupt::LCD => 1 << 1,
            Interrupt::Timer => 1 << 2,
            Interrupt::Serial => 1 << 3,
            Interrupt::Joypad => 1 << 4,
        }
    }
}
