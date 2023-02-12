use super::{Header, Mapper};

pub struct Nrom {
    nrom256: bool,
}

impl Nrom {
    pub fn new(header: Header) -> Box<Self> {
        Box::new(Nrom { nrom256: header.prg_rom_size == 0x8000 })
    }
}

impl Mapper for Nrom {
    fn translate_address(&self, addr: usize) -> usize {
        assert!(addr >= 0x8000);
        if self.nrom256 {addr} else {addr & 0xbfff}
    }
}