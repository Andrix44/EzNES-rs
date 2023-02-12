mod nrom;

pub enum Mirroring {
    Vertical,
    Horizontal,
}

pub enum ConsoleType {
    NesFamicom,
    VsSystem,
    Playchoice,
    Extended,
}

pub enum Region {
    NTSC,
    PAL,
}

pub struct Header {
    pub version2: bool, // Unimplemented
    pub prg_ram_size: u16,
    pub prg_rom_size: u16,
    pub chr_ram_size: u16,
    pub chr_rom_size: u16,
    pub mirroring: Mirroring,
    pub prg_ram_battery: bool,
    pub trainer: bool,
    pub ignore_mirroring: bool,
    pub mapper: u8,
    pub console_type: ConsoleType,
    pub region: Region,
}

pub fn parse_header(header_bytes: [u8; 16]) ->  Result<Header, &'static str> {
    if header_bytes[..4].as_ref() != b"NES\x1a" {
        return Err("Wrong header magic!");
    };

    let prg_rom_size = header_bytes[4] as u16 * 0x4000;
    let chr_rom_size = header_bytes[5] as u16 * 0x2000;
    let chr_ram_size = match chr_rom_size {
        0 => 0x2000,
        _ => 0
    };
    let mirroring = match header_bytes[6] & 1 {
        0 => Mirroring::Horizontal,
        1 => Mirroring::Vertical,
        _ => unreachable!()
    };
    let prg_ram_battery = header_bytes[6] & 2 != 0;
    let trainer = header_bytes[6] & 4 != 0;
    let ignore_mirroring = header_bytes[6] & 8 != 0;

    let version2 = (header_bytes[7] >> 2) & 0x3 == 0x2;
    if !version2 && header_bytes[11..].as_ref() == b"\x00\x00\x00\x00" {
        let mapper = (header_bytes[6] >> 4) | (header_bytes[7] & 0xf0);
        let console_type = match header_bytes[7] & 3 {
            0 => ConsoleType::NesFamicom,
            1 => ConsoleType::VsSystem,
            2 => ConsoleType::Playchoice,
            3 => ConsoleType::Extended,
            _ => unreachable!()
        };
        let prg_ram_size = std::cmp::max(header_bytes[8] as u16 * 0x2000, 0x2000);
        let region = match header_bytes[9] & 1 {
            0 => Region::NTSC,
            1 => Region::PAL,
            _ => unreachable!()
        };

        Ok(Header {
            version2,
            prg_ram_size,
            prg_rom_size,
            chr_ram_size,
            chr_rom_size,
            mirroring,
            prg_ram_battery,
            trainer,
            ignore_mirroring,
            mapper,
            console_type,
            region,
        })
    }
    else {
        let mapper = header_bytes[6] >> 4;
        let console_type = ConsoleType::NesFamicom;
        let prg_ram_size = 0x2000;
        let region = Region::NTSC;

        Ok(Header { version2, prg_ram_size, prg_rom_size, chr_ram_size, chr_rom_size, mirroring, prg_ram_battery, trainer, ignore_mirroring, mapper, console_type, region })
    }

}

trait Mapper {
    fn translate_address(&self, addr: usize) -> usize;
}

pub struct MemoryBus {
    mem: [u8; 0xffff],
    mapper: Box<dyn Mapper>
}

impl MemoryBus {
    pub fn new(header: Header, prg_rom_data: &[u8]) -> Self {
        let mut mem = [0; 0xffff];
        mem[0x8000 .. (0x8000 + prg_rom_data.len())].copy_from_slice(&prg_rom_data[..]);

        let mapper = match header.mapper {
            0 => nrom::Nrom::new(header),
            _ => unimplemented!()
        };

        MemoryBus { mem, mapper }
    }

    pub fn read(&self, addr: usize) -> u8 {
        if addr < 0x2000 {
            self.mem[addr & 0x7ff]
        }
        else if addr < 0x4000 {
            self.mem[addr & 7]
        }
        else if addr < 0x4016 {
            self.mem[addr]
        }
        else if addr < 0x4018 {
            self.mem[addr]
        }
        else if addr < 0x8000 {
            self.mem[addr]
        }
        else {
            self.mem[self.mapper.translate_address(addr)]
        }
    }

    pub fn write(&mut self, addr: usize, data: u8) {
        if addr < 0x2000 {
            self.mem[addr & 0x7ff] = data
        }
        else if addr < 0x4000 {
            self.mem[addr & 7] = data
        }
        else if addr < 0x4016 {
            self.mem[addr] = data
        }
        else if addr < 0x4018 {
            self.mem[addr] = data
        }
        else if addr < 0x8000 {
            self.mem[addr] = data
        }
        else {
            self.mem[self.mapper.translate_address(addr)] = data
        }
    }
}
