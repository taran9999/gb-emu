#[derive(Default)]
pub struct APU {
    audio_master_control: u8,
    sound_panning: u8,
    master_volume: u8,
    channel_1: [u8; 5], // sweep, timer, volume, period low, period high
    channel_2: [u8; 4], // timer, volume, period low, period high
    channel_3: [u8; 5], // DAC enable, timer, output level, period low, period high
    wave_ram: [u8; 16],
    channel_4: [u8; 4], // timer, volume, frequency, control
}

impl APU {
    pub fn new() -> APU {
        APU::default()
    }

    pub fn apu_read(&self, address: usize) -> u8 {
        match address {
            0xFF26 => self.audio_master_control,
            _ => panic!("APU read out of bounds at ${:04X}", address),
        }
    }

    pub fn apu_write(&mut self, address: usize, value: u8) {
        match address {
            // TODO: CHns should not be changed, should the value be masked?
            0xFF26 => self.audio_master_control = value,
            _ => panic!("APU write out of bounds at ${:04X}", address),
        }
    }
}
