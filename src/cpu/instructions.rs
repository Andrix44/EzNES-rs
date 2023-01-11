use crate::cpu::{CPU, AddressingModes};

struct InstrEntry {
    pub name: &'static str,
    pub handler: fn(&mut CPU, &AddressingModes),
    pub opcode: u8,
    pub mode: AddressingModes,
    pub len: u8,
    pub cycles: u8,
}

const instr_list: [InstrEntry; 151] = [
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x69, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x65, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x75, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x6d, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x7d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x79, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x61, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "ADC", handler: CPU::adc, opcode: 0x71, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x29, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x25, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x35, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x2d, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x3d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x39, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x21, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "AND", handler: CPU::and, opcode: 0x31, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "ASL", handler: CPU::asl, opcode: 0x0a, mode: AddressingModes::Accumulator, len: 1, cycles: 2},
    InstrEntry {name: "ASL", handler: CPU::asl, opcode: 0x06, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "ASL", handler: CPU::asl, opcode: 0x16, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "ASL", handler: CPU::asl, opcode: 0x0e, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "ASL", handler: CPU::asl, opcode: 0x1e, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "BCC", handler: CPU::bcc, opcode: 0x90, mode: AddressingModes::Relative, len: 2, cycles: 2},
    InstrEntry {name: "BCS", handler: CPU::bcs, opcode: 0xb0, mode: AddressingModes::Relative, len: 2, cycles: 2},
    
    InstrEntry {name: "BNE", handler: CPU::bne, opcode: 0xd0, mode: AddressingModes::Relative, len: 2, cycles: 2},
    InstrEntry {name: "BEQ", handler: CPU::beq, opcode: 0xf0, mode: AddressingModes::Relative, len: 2, cycles: 2},
    
    InstrEntry {name: "BIT", handler: CPU::bit, opcode: 0x24, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "BIT", handler: CPU::bit, opcode: 0x2c, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    
    InstrEntry {name: "BPL", handler: CPU::bpl, opcode: 0x10, mode: AddressingModes::Relative, len: 2, cycles: 2},
    InstrEntry {name: "BMI", handler: CPU::bmi, opcode: 0x30, mode: AddressingModes::Relative, len: 2, cycles: 2},
    
    InstrEntry {name: "BRK", handler: CPU::brk, opcode: 0x00, mode: AddressingModes::Implicit, len: 1, cycles: 7},
    
    InstrEntry {name: "BVC", handler: CPU::bvc, opcode: 0x50, mode: AddressingModes::Relative, len: 2, cycles: 2},
    InstrEntry {name: "BVS", handler: CPU::bvs, opcode: 0x70, mode: AddressingModes::Relative, len: 2, cycles: 2},
    
    InstrEntry {name: "CLC", handler: CPU::clc, opcode: 0x18, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "CLD", handler: CPU::cld, opcode: 0xd8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "CLI", handler: CPU::cli, opcode: 0x58, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "CLV", handler: CPU::clv, opcode: 0xb8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xc9, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xc5, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xd5, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xcd, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xdd, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xd9, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xc1, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "CMD", handler: CPU::cmp, opcode: 0xd1, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "CPX", handler: CPU::cpx, opcode: 0xe0, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "CPX", handler: CPU::cpx, opcode: 0xe4, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "CPX", handler: CPU::cpx, opcode: 0xec, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    
    InstrEntry {name: "CPY", opcode: 0xc0, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "CPY", opcode: 0xc4, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "CPY", opcode: 0xcc, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    
    InstrEntry {name: "DEC", opcode: 0xc6, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "DEC", opcode: 0xd6, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "DEC", opcode: 0xce, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "DEC", opcode: 0xde, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "DEX", opcode: 0xca, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "DEY", opcode: 0x88, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "EOR", opcode: 0x49, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "EOR", opcode: 0x45, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "EOR", opcode: 0x55, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "EOR", opcode: 0x4d, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "EOR", opcode: 0x5d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "EOR", opcode: 0x59, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "EOR", opcode: 0x41, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "EOR", opcode: 0x51, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},

    InstrEntry {name: "INC", opcode: 0xe6, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "INC", opcode: 0xf6, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "INC", opcode: 0xee, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "INC", opcode: 0xfe, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "INX", opcode: 0xe8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "INY", opcode: 0xc8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "JMP", opcode: 0x4c, mode: AddressingModes::Absolute, len: 3, cycles: 3},
    InstrEntry {name: "JMP", opcode: 0x6c, mode: AddressingModes::Indirect, len: 3, cycles: 5},
    
    InstrEntry {name: "JSR", opcode: 0x20, mode: AddressingModes::Absolute, len: 3, cycles: 6},

    InstrEntry {name: "LDA", opcode: 0xa9, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "LDA", opcode: 0xa5, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "LDA", opcode: 0xb5, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "LDA", opcode: 0xad, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "LDA", opcode: 0xbd, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "LDA", opcode: 0xb9, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "LDA", opcode: 0xa1, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "LDA", opcode: 0xb1, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},

    InstrEntry {name: "LDX", opcode: 0xa2, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "LDX", opcode: 0xa6, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "LDX", opcode: 0xb6, mode: AddressingModes::ZeroPageY, len: 2, cycles: 4},
    InstrEntry {name: "LDX", opcode: 0xae, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "LDX", opcode: 0xbe, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},

    InstrEntry {name: "LDY", opcode: 0xa0, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "LDY", opcode: 0xa4, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "LDY", opcode: 0xb4, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "LDY", opcode: 0xac, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "LDY", opcode: 0xbc, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},

    InstrEntry {name: "LSR", opcode: 0x4a, mode: AddressingModes::Accumulator, len: 1, cycles: 2},
    InstrEntry {name: "LSR", opcode: 0x46, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "LSR", opcode: 0x56, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "LSR", opcode: 0x4e, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "LSR", opcode: 0x5e, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "NOP", opcode: 0xea, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "ORA", opcode: 0x09, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "ORA", opcode: 0x05, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "ORA", opcode: 0x15, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "ORA", opcode: 0x0d, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "ORA", opcode: 0x1d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "ORA", opcode: 0x19, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "ORA", opcode: 0x01, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "ORA", opcode: 0x11, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "PHA", opcode: 0x48, mode: AddressingModes::Implicit, len: 1, cycles: 3},
    InstrEntry {name: "PLA", opcode: 0x68, mode: AddressingModes::Implicit, len: 1, cycles: 4},
    
    InstrEntry {name: "PHP", opcode: 0x08, mode: AddressingModes::Implicit, len: 1, cycles: 3},
    InstrEntry {name: "PLP", opcode: 0x28, mode: AddressingModes::Implicit, len: 1, cycles: 4},
    
    InstrEntry {name: "ROL", opcode: 0x2a, mode: AddressingModes::Accumulator, len: 1, cycles: 2},
    InstrEntry {name: "ROL", opcode: 0x26, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "ROL", opcode: 0x36, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "ROL", opcode: 0x2e, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "ROL", opcode: 0x3e, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "ROR", opcode: 0x6a, mode: AddressingModes::Accumulator, len: 1, cycles: 2},
    InstrEntry {name: "ROR", opcode: 0x66, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "ROR", opcode: 0x76, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "ROR", opcode: 0x6e, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "ROR", opcode: 0x7e, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "RTI", opcode: 0x40, mode: AddressingModes::Implicit, len: 1, cycles: 6},
    InstrEntry {name: "RTS", opcode: 0x60, mode: AddressingModes::Implicit, len: 1, cycles: 6},

    InstrEntry {name: "SBC", opcode: 0xe9, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "SBC", opcode: 0xe5, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "SBC", opcode: 0xf5, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "SBC", opcode: 0xed, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "SBC", opcode: 0xfd, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "SBC", opcode: 0xf9, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "SBC", opcode: 0xe1, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "SBC", opcode: 0xf1, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "SEC", opcode: 0x38, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "SED", opcode: 0xf8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "SEI", opcode: 0x78, mode: AddressingModes::Implicit, len: 1, cycles: 2},

    InstrEntry {name: "STA", opcode: 0x85, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "STA", opcode: 0x95, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "STA", opcode: 0x8d, mode: AddressingModes::Absolute, len: 3, cycles: 5},
    InstrEntry {name: "STA", opcode: 0x9d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 5},
    InstrEntry {name: "STA", opcode: 0x99, mode: AddressingModes::AbsoluteY, len: 3, cycles: 5},
    InstrEntry {name: "STA", opcode: 0x81, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "STA", opcode: 0x91, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 6},
    
    InstrEntry {name: "STX", opcode: 0x86, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "STX", opcode: 0x96, mode: AddressingModes::ZeroPageY, len: 2, cycles: 4},
    InstrEntry {name: "STX", opcode: 0x8e, mode: AddressingModes::Absolute, len: 3, cycles: 4},

    InstrEntry {name: "STY", opcode: 0x84, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "STY", opcode: 0x94, mode: AddressingModes::ZeroPageY, len: 2, cycles: 4},
    InstrEntry {name: "STY", opcode: 0x8c, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    
    InstrEntry {name: "TAX", opcode: 0xaa, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "TAY", opcode: 0xa8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "TSX", opcode: 0xba, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "TXS", opcode: 0x9a, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "TXA", opcode: 0x8a, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "TYA", opcode: 0x98, mode: AddressingModes::Implicit, len: 1, cycles: 2},
];

pub fn get_instr(opcode: u8) -> Option<&'static InstrEntry> {
    instr_list.iter().find(|instr| instr.opcode == opcode)
}

impl CPU {
    fn adc(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        let orig_sign = self.A >> 7;
        let mut carry1: bool;
        let mut carry2: bool;
        (self.A, carry1) = self.A.overflowing_add(data);
        (self.A, carry2) = self.A.overflowing_add(self.flags.carry as u8);
        self.flags.carry = carry1 || carry2;
        self.flags.zero = self.A == 0;
        self.flags.overflow = (orig_sign == data >> 7) && (orig_sign != self.A >> 7);
        self.flags.negative = self.A >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn and(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);
        
        self.A = self.A & data;
        self.flags.zero = self.A == 0;
        self.flags.negative = self.A >> 7 == 1;

        if page_crossed {
            self.cycles += 1
        };
    }

    fn asl(&mut self, mode: &AddressingModes) {
        match mode {
            AddressingModes::Accumulator => {
                let data = self.A;
                self.flags.carry = data >> 7 == 1;
                self.A = data << 1;
                self.flags.zero = self.A == 0;
                self.flags.negative = self.A >> 7 == 1;
            }
            _ => {
                let (addr, _) = self.get_operand_addr(mode);
                let data = self.mem[addr as usize];
                self.flags.carry = data >> 7 == 1;
                self.mem[addr as usize] = data << 1;
                self.flags.zero = self.mem[addr as usize] == 0;
                self.flags.negative = self.mem[addr as usize] >> 7 == 1;
            }
        }
    }

    fn bcc(&mut self, _mode: &AddressingModes) {
        if !self.flags.carry {
            self.do_relative_jump();
        }
    }

    fn bcs(&mut self, _mode: &AddressingModes) {
        if self.flags.carry {
            self.do_relative_jump();
        }
    }

    fn beq(&mut self, _mode: &AddressingModes) {
        if self.flags.zero {
            self.do_relative_jump();
        }
    }

    fn bne(&mut self, _mode: &AddressingModes) {
        if !self.flags.zero {
            self.do_relative_jump();
        }
    }

    fn bit(&mut self, mode: &AddressingModes) {
        let (data, _) = self.get_data(mode);
        self.flags.zero = self.A & data == 0;
        self.flags.overflow = data >> 7 == 1;
        self.flags.negative = (data >> 6) & 1 == 1;
    }

    fn bmi(&mut self, _mode: &AddressingModes) {
        if self.flags.negative {
            self.do_relative_jump();
        }
    }

    fn bpl(&mut self, _mode: &AddressingModes) {
        if !self.flags.negative {
            self.do_relative_jump();
        }
    }

    fn brk(&mut self, _mode: &AddressingModes) {
        self.push((self.pc + 1 & 0xff) as u8); // different sources say Different things about this instruction
        self.push((self.pc + 1 >> 8) as u8);

        self.flags.breakfl = true;
        //self.flags.interrupt_disable = true;
        self.push(self.flags.get_u8());

        self.pc = self.read_mem_u16(0xfffe);

        self.pc_autoincrement = false;
    }

    fn bvs(&mut self, _mode: &AddressingModes) {
        if self.flags.overflow {
            self.do_relative_jump();
        }
    }

    fn bvc(&mut self, _mode: &AddressingModes) {
        if !self.flags.overflow {
            self.do_relative_jump();
        }
    }

    fn clc(&mut self, _mode: &AddressingModes) {
        self.flags.carry = false;
    }

    fn cld(&mut self, _mode: &AddressingModes) {
        self.flags.decimal_mode = false;
    }

    fn cli(&mut self, _mode: &AddressingModes) {
        self.flags.interrupt_disable = false;
    }

    fn clv(&mut self, _mode: &AddressingModes) {
        self.flags.overflow = false;
    }

    fn cmp(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.flags.carry = self.A >= data;
        self.flags.carry = self.A == data;
        self.flags.negative = (self.A - data) >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn cpx(&mut self, mode: &AddressingModes) {
        let (data, _) = self.get_data(mode);

        self.flags.carry = self.X >= data;
        self.flags.carry = self.X == data;
        self.flags.negative = (self.X - data) >> 7 == 1;
    }
    
}