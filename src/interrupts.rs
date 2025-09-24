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

    pub fn get_first(flags: u8) -> Option<Interrupt> {
        if flags & 1 == 1 {
            return Some(Interrupt::VBlank);
        }

        if (flags >> 1) & 1 == 1 {
            return Some(Interrupt::LCD);
        }

        if (flags >> 2) & 1 == 1 {
            return Some(Interrupt::Timer);
        }

        if (flags >> 3) & 1 == 1 {
            return Some(Interrupt::Serial);
        }

        if (flags >> 4) & 1 == 1 {
            return Some(Interrupt::Joypad);
        }

        None
    }
}
