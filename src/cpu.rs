use self::instructions::get_instr;

mod instructions;
use crate::mem::{MemoryBus, parse_header};

#[derive(PartialEq)]
pub enum AddressingModes {
    Implicit,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed
}

struct Flags {
    carry: bool,
    zero: bool,
    interrupt_disable: bool,
    decimal_mode: bool,
    breakfl: bool,
    unused: bool,
    overflow: bool,
    negative: bool,
}

impl Flags {
    pub fn get_u8(&self) -> u8 {
        (self.carry as u8) + (self.zero as u8) << 1 + (self.interrupt_disable as u8) << 2 + (self.decimal_mode as u8) << 3 +
        (self.breakfl as u8) << 4 + (self.unused as u8) << 5 + (self.overflow as u8) << 6 + (self.negative as u8) << 7
    }

    pub fn set_u8(&mut self, data: u8) {
        self.carry = (data >> 0) & 1 == 1;
        self.zero = (data >> 1) & 1 == 1;
        self.interrupt_disable = (data >> 2) & 1 == 1;
        self.decimal_mode = (data >> 3) & 1 == 1;
        self.breakfl = (data >> 4) & 1 == 1;
        self.unused = (data >> 5) & 1 == 1;
        self.overflow = (data >> 6) & 1 == 1;
        self.negative = (data >> 7) & 1 == 1;
    }
}

pub struct CPU {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    flags: Flags,
    mem: MemoryBus,
    pc_autoincrement: bool,
    cycles: u64,
}

impl CPU {
    pub fn new(path: &str) -> Self {
        let bytes = std::fs::read(path).expect("Error while reading file!");
        let header = parse_header(bytes[..16].try_into().expect("Input file is shorter than 15 bytes!")).unwrap();
        let prg_rom_data = if header.trainer {&bytes[16 + 512 .. 16 + 512 + header.prg_rom_size as usize]} 
                                    else {&bytes[16 .. 16 + header.prg_rom_size as usize]};
        let mem = MemoryBus::new(header, prg_rom_data);
        let lo = mem.read(0xfffc);
        let hi = mem.read(0xfffd);
        let pc = u16::from_le_bytes([lo, hi]);

        CPU {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xfd,
            pc,
            flags: Flags { // TODO: check that this is 0b00110100
                carry: false,
                zero: false,
                interrupt_disable: true,
                decimal_mode: false,
                breakfl: true,
                unused: true,
                overflow: false,
                negative: false,
            },
            mem,
            pc_autoincrement: false,
            cycles: 0,
        }
    }

    fn read_mem(&self, addr: u16) -> u8 {
        self.mem.read(addr as usize)
    }

    fn read_mem_u16(&self, addr: u16) -> u16 {
        let lo = self.read_mem(addr);
        let hi = self.read_mem(addr + 1);
        u16::from_le_bytes([lo, hi])
    }

    fn write_mem(&mut self, addr: u16, data: u8) {
        self.mem.write(addr as usize, data)
    }

    fn write_mem_u16(&mut self, addr: u16, data: u16) {
        self.write_mem(addr, data as u8);
        self.write_mem(addr + 1, (data >> 8) as u8)
    }

    // also returns whether a page was crossed or not (for cycle calculation)
    fn get_operand_addr(&self, mode: &AddressingModes) -> (u16, bool) {
        match mode {
            AddressingModes::Implicit => panic!("There is no need to call this method fo implicit addressing!"),
            AddressingModes::Accumulator => panic!("There is no need to call this method fo accumulator addressing!"),
            AddressingModes::Immediate => (self.pc + 1, false), // Incremented after opcode fetch
            AddressingModes::ZeroPage => (self.read_mem(self.pc + 1) as u16, false),
            AddressingModes::ZeroPageX => (self.read_mem(self.pc + 1).wrapping_add(self.x) as u16, false),
            AddressingModes::ZeroPageY => (self.read_mem(self.pc + 1).wrapping_add(self.y) as u16, false),
            AddressingModes::Relative => {
                let ret = (self.pc + 2).wrapping_add_signed(self.read_mem(self.pc + 1) as i16);
                (ret, (self.pc >> 8) != (ret >> 8))
            },
            AddressingModes::Absolute => (self.read_mem_u16(self.pc + 1), false),
            AddressingModes::AbsoluteX => {
                let abs = self.read_mem_u16(self.pc + 1);
                let ret = abs.wrapping_add(self.y as u16);
                (ret, (abs >> 8) != (ret >> 8))
            },
            AddressingModes::AbsoluteY => {
                let abs = self.read_mem_u16(self.pc + 1);
                let ret = abs.wrapping_add(self.x as u16);
                (ret, (abs >> 8) != (ret >> 8))
            },
            AddressingModes::Indirect => (self.read_mem_u16(self.read_mem_u16(self.pc + 1)), false),
            AddressingModes::IndexedIndirect => (self.read_mem_u16(self.read_mem(self.pc + 1).wrapping_add(self.x) as u16), false),
            AddressingModes::IndirectIndexed => {
                let ind = self.read_mem_u16(self.read_mem(self.pc + 1) as u16);
                let ret = ind.wrapping_add(self.y as u16);
                (ret, (ind >> 8) != (ret >> 8))
            }
        }
    }

    // also returns whether a page was crossed or not (for cycle calculation)
    fn get_data(&self, mode: &AddressingModes) -> (u8, bool) {
        let (addr, page_crossed) = self.get_operand_addr(&mode);
        (self.read_mem(addr), page_crossed)
    }

    fn do_relative_jump(&mut self) {
        let page_crossed: bool;
        (self.pc, page_crossed) = self.get_operand_addr(&AddressingModes::Relative);
        self.pc_autoincrement = false;      
        self.cycles += 1;
        if page_crossed {
            self.cycles += 1;
        }
    }

    fn push(&mut self, data: u8) {
        self.write_mem(self.sp as u16 + 0x100, data);
        self.sp -= 1;
    }

    fn push16(&mut self, data: u16) {
        self.write_mem_u16(self.sp as u16 - 1 + 0x100, data);
        self.sp -= 2;
    }

    fn pop(&mut self) -> u8 {
        self.sp += 1;
        self.read_mem(self.sp as u16 + 0x100)
    }

    fn pop16(&mut self) -> u16 {
        self.sp += 2;
        self.read_mem_u16(self.sp as u16 - 1 + 0x100)
    }

    pub fn run(&mut self){
        loop {
            self.pc_autoincrement = true;

            let opcode = self.read_mem(self.pc);
            let instr = get_instr(opcode).expect(format!("Illegal instruction hit: {}", opcode).as_str());
            /* let op1 = self.read_mem(self.pc + 1).to_string().as_str();
            let op2 = self.read_mem(self.pc + 2).to_string().as_str();
            let op = format!("{} {}", op1, op2);
            let operands = match instr.len - 1 {
                0 => "",
                1 => op1,
                2 => op2,
                _ => unreachable!()
            };
            println!("{} {}", instr.name, operands); */
            println!("{}", instr.name);
            (instr.handler)(self, &instr.mode);

            self.cycles += instr.cycles as u64;

            if self.pc_autoincrement {
                self.pc += instr.len as u16;
            }
        }
    }

    fn reset(&mut self){
        self.pc = self.read_mem_u16(0xfffc);
        self.sp -= 3;
        self.flags.interrupt_disable = true;
    }
}