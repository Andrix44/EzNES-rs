use std::ops::{Index, IndexMut};

enum Mirroring {
    Vertical,
    Horizontal,
}

enum ConsoleType {
    NesFamicom,
    VsSystem,
    Playchoice,
    Extended,
}

enum Region {
    NTSC,
    PAL,
}

struct Header {
    version2: bool, // Unimplemented
    prg_ram_size: u16,
    prg_rom_size: u16,
    chr_ram_size: u16,
    chr_rom_size: u16,
    mirroring: Mirroring,
    prg_ram_battery: bool,
    trainer: bool,
    ignore_mirroring: bool,
    mapper: u8,
    console_type: ConsoleType,
    region: Region,
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
        1 => Mirroring::Vertical
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
            3 => ConsoleType::Extended
        };
        let prg_ram_size = std::cmp::max(header_bytes[8] as u16 * 0x2000, 0x2000);
        let region = match header_bytes[9] & 1 {
            0 => Region::NTSC,
            1 => Region::PAL
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

pub struct Memory {
    header: Header,
    cpu_mem: Vec<u8>,
    prg_mem: Vec<u8>,
    chr_mem: Vec<u8>,
    cpu_ram: Vec<u8>,
}

impl Memory {
    pub fn new(rom_data: [u8; 16]) -> Self {
        let header = parse_header(rom_data).unwrap(); // TODO: this should fail silently when the file open dialog is implemented
        let cpu_mem = vec![0; 0xffff];
        let prg_mem = vec![0; header.prg_rom_size as usize];
        let chr_mem = vec![0; header.chr_rom_size as usize];
        let cpu_ram = vec![0; 0x800];

        Memory { header, cpu_mem, prg_mem, chr_mem, cpu_ram }
    }
}

impl Index<usize> for Memory {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.cpu_mem[index]
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.cpu_mem[index]
    }
}