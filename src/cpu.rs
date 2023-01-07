use self::instructions::get_instr;

mod instructions;

enum AddressingModes {
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
    break_command: bool,
    unused: bool,
    overflow: bool,
    negative: bool,
}

pub struct CPU {
    A: u8,
    X: u8,
    Y: u8,
    sp: u8,
    pc: u16,
    flags: Flags,
    mem: [u8; 0xffff],
    pc_autoincrement: bool,
    cycles: u64,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            A: 0,
            X: 0,
            Y: 0,
            sp: 0xfd,
            pc: 0,
            flags: Flags { // TODO: check that this is 0b00110100
                carry: false,
                zero: false,
                interrupt_disable: true,
                decimal_mode: true,
                break_command: false,
                unused: false,
                overflow: false,
                negative: false,
            },
            mem: [0; 0xffff],
            pc_autoincrement: false,
            cycles: 0,
        }
    }

    fn read_mem(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    fn read_mem_u16(&self, addr: u16) -> u16 {
        let lo = self.read_mem(addr);
        let hi = self.read_mem(addr + 1);
        u16::from_le_bytes([lo, hi])
    }

    fn write_mem(&mut self, addr: u16, val: u8) {
        self.mem[addr as usize] = val;
    }

    fn write_mem_u16(&mut self, addr: u16, val: u16) {
        self.write_mem(addr, val as u8);
        self.write_mem(addr + 1, (val >> 8) as u8);
    }

    // also returns whether a page was crossed or not (for cycle calculation)
    fn get_operand_addr(&self, mode: &AddressingModes) -> (u16, bool) {
        match mode {
            AddressingModes::Implicit => panic!("There is no need to call this method fo implicit addressing!"),
            AddressingModes::Accumulator => panic!("There is no need to call this method fo accumulator addressing!"),
            AddressingModes::Immediate => (self.pc + 1, false), // Incremented after opcode fetch
            AddressingModes::ZeroPage => (self.read_mem(self.pc + 1) as u16, false),
            AddressingModes::ZeroPageX => (self.read_mem(self.pc + 1).wrapping_add(self.X) as u16, false),
            AddressingModes::ZeroPageY => (self.read_mem(self.pc + 1).wrapping_add(self.Y) as u16, false),
            AddressingModes::Relative => {
                let ret = (self.pc + 2).wrapping_add_signed(self.read_mem(self.pc + 1) as i16);
                (ret, (self.pc >> 8) != (ret >> 8))
            },
            AddressingModes::Absolute => (self.read_mem_u16(self.pc + 1), false),
            AddressingModes::AbsoluteX => {
                let abs = self.read_mem_u16(self.pc + 1);
                let ret = abs.wrapping_add(self.Y as u16);
                (ret, (abs >> 8) != (ret >> 8))
            },
            AddressingModes::AbsoluteY => {
                let abs = self.read_mem_u16(self.pc + 1);
                let ret = abs.wrapping_add(self.X as u16);
                (ret, (abs >> 8) != (ret >> 8))
            },
            AddressingModes::Indirect => (self.read_mem_u16(self.read_mem_u16(self.pc + 1)), false),
            AddressingModes::IndexedIndirect => (self.read_mem_u16(self.read_mem(self.pc + 1).wrapping_add(self.X) as u16), false),
            AddressingModes::IndirectIndexed => {
                let ind = self.read_mem_u16(self.read_mem(self.pc + 1) as u16);
                let ret = ind.wrapping_add(self.Y as u16);
                (ret, (ind >> 8) != (ret >> 8))
            }
        }
    }

    fn get_data(&self, mode: &AddressingModes) -> (u8, bool) {
        let (addr, page_crossed) = self.get_operand_addr(&mode);
        (self.mem[addr as usize], page_crossed)
    }

    fn run(&mut self){
        loop {
            self.pc_autoincrement = true;

            let opcode = self.read_mem(self.pc);
            let instr = get_instr(opcode).expect(format!("Illegal instruction hit: {}", opcode).as_str());
            (instr.handler)(&mut self, &instr.mode);

            self.cycles += instr.cycles as u64;

            if self.pc_autoincrement {
                self.pc += instr.len as u16;
            }
        }
    }

    fn load_rom(&mut self, path: &str){
        let bytes = std::fs::read(path).expect("Error while reading file!");
        self.mem[0x8000 .. (0x8000 + bytes.len())].copy_from_slice(&bytes[..]);
    }

    fn reset(&mut self){
        self.pc = self.read_mem_u16(0xfffc);
        self.sp -= 3;
        self.flags.interrupt_disable = true;
    }

    pub fn run_rom(&mut self, path: &str){
        self.load_rom(path);
        self.reset();
        self.run();
    }
}