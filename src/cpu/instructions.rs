use crate::cpu::{CPU, AddressingModes};

pub struct InstrEntry {
    pub name: &'static str,
    pub handler: fn(&mut CPU, &AddressingModes),
    pub opcode: u8,
    pub mode: AddressingModes,
    pub len: u8,
    pub cycles: u8,
}

const INSTR_LIST: [InstrEntry; 185] = [
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
    
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xc9, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xc5, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xd5, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xcd, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xdd, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xd9, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xc1, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "CMP", handler: CPU::cmp, opcode: 0xd1, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "CPX", handler: CPU::cpx, opcode: 0xe0, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "CPX", handler: CPU::cpx, opcode: 0xe4, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "CPX", handler: CPU::cpx, opcode: 0xec, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    
    InstrEntry {name: "CPY", handler: CPU::cpy, opcode: 0xc0, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "CPY", handler: CPU::cpy, opcode: 0xc4, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "CPY", handler: CPU::cpy, opcode: 0xcc, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    
    InstrEntry {name: "DEC", handler: CPU::dec, opcode: 0xc6, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "DEC", handler: CPU::dec, opcode: 0xd6, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "DEC", handler: CPU::dec, opcode: 0xce, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "DEC", handler: CPU::dec, opcode: 0xde, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "DEX", handler: CPU::dex, opcode: 0xca, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "DEY", handler: CPU::dey, opcode: 0x88, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x49, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x45, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x55, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x4d, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x5d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x59, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x41, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "EOR", handler: CPU::eor, opcode: 0x51, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},

    InstrEntry {name: "INC", handler: CPU::inc, opcode: 0xe6, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "INC", handler: CPU::inc, opcode: 0xf6, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "INC", handler: CPU::inc, opcode: 0xee, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "INC", handler: CPU::inc, opcode: 0xfe, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "INX", handler: CPU::inx, opcode: 0xe8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "INY", handler: CPU::iny, opcode: 0xc8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "JMP", handler: CPU::jmp, opcode: 0x4c, mode: AddressingModes::Absolute, len: 3, cycles: 3},
    InstrEntry {name: "JMP", handler: CPU::jmp, opcode: 0x6c, mode: AddressingModes::Indirect, len: 3, cycles: 5},
    
    InstrEntry {name: "JSR", handler: CPU::jsr, opcode: 0x20, mode: AddressingModes::Absolute, len: 3, cycles: 6},

    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xa9, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xa5, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xb5, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xad, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xbd, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xb9, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xa1, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "LDA", handler: CPU::lda, opcode: 0xb1, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},

    InstrEntry {name: "LDX", handler: CPU::ldx, opcode: 0xa2, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "LDX", handler: CPU::ldx, opcode: 0xa6, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "LDX", handler: CPU::ldx, opcode: 0xb6, mode: AddressingModes::ZeroPageY, len: 2, cycles: 4},
    InstrEntry {name: "LDX", handler: CPU::ldx, opcode: 0xae, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "LDX", handler: CPU::ldx, opcode: 0xbe, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},

    InstrEntry {name: "LDY", handler: CPU::ldy, opcode: 0xa0, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "LDY", handler: CPU::ldy, opcode: 0xa4, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "LDY", handler: CPU::ldy, opcode: 0xb4, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "LDY", handler: CPU::ldy, opcode: 0xac, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "LDY", handler: CPU::ldy, opcode: 0xbc, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},

    InstrEntry {name: "LSR", handler: CPU::lsr, opcode: 0x4a, mode: AddressingModes::Accumulator, len: 1, cycles: 2},
    InstrEntry {name: "LSR", handler: CPU::lsr, opcode: 0x46, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "LSR", handler: CPU::lsr, opcode: 0x56, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "LSR", handler: CPU::lsr, opcode: 0x4e, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "LSR", handler: CPU::lsr, opcode: 0x5e, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "NOP", handler: CPU::nop, opcode: 0xea, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x09, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x05, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x15, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x0d, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x1d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x19, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x01, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "ORA", handler: CPU::ora, opcode: 0x11, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "PHA", handler: CPU::pha, opcode: 0x48, mode: AddressingModes::Implicit, len: 1, cycles: 3},
    InstrEntry {name: "PLA", handler: CPU::pla, opcode: 0x68, mode: AddressingModes::Implicit, len: 1, cycles: 4},
    
    InstrEntry {name: "PHP", handler: CPU::php, opcode: 0x08, mode: AddressingModes::Implicit, len: 1, cycles: 3},
    InstrEntry {name: "PLP", handler: CPU::plp, opcode: 0x28, mode: AddressingModes::Implicit, len: 1, cycles: 4},
    
    InstrEntry {name: "ROL", handler: CPU::rol, opcode: 0x2a, mode: AddressingModes::Accumulator, len: 1, cycles: 2},
    InstrEntry {name: "ROL", handler: CPU::rol, opcode: 0x26, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "ROL", handler: CPU::rol, opcode: 0x36, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "ROL", handler: CPU::rol, opcode: 0x2e, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "ROL", handler: CPU::rol, opcode: 0x3e, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "ROR", handler: CPU::ror, opcode: 0x6a, mode: AddressingModes::Accumulator, len: 1, cycles: 2},
    InstrEntry {name: "ROR", handler: CPU::ror, opcode: 0x66, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry {name: "ROR", handler: CPU::ror, opcode: 0x76, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry {name: "ROR", handler: CPU::ror, opcode: 0x6e, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry {name: "ROR", handler: CPU::ror, opcode: 0x7e, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    
    InstrEntry {name: "RTI", handler: CPU::rti, opcode: 0x40, mode: AddressingModes::Implicit, len: 1, cycles: 6},
    InstrEntry {name: "RTS", handler: CPU::rts, opcode: 0x60, mode: AddressingModes::Implicit, len: 1, cycles: 6},

    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xe9, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xe5, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xf5, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xed, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xfd, mode: AddressingModes::AbsoluteX, len: 3, cycles: 4},
    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xf9, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xe1, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "SBC", handler: CPU::sbc, opcode: 0xf1, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},
    
    InstrEntry {name: "SEC", handler: CPU::sec, opcode: 0x38, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "SED", handler: CPU::sed, opcode: 0xf8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "SEI", handler: CPU::sei, opcode: 0x78, mode: AddressingModes::Implicit, len: 1, cycles: 2},

    InstrEntry {name: "STA", handler: CPU::sta, opcode: 0x85, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "STA", handler: CPU::sta, opcode: 0x95, mode: AddressingModes::ZeroPageX, len: 2, cycles: 4},
    InstrEntry {name: "STA", handler: CPU::sta, opcode: 0x8d, mode: AddressingModes::Absolute, len: 3, cycles: 5},
    InstrEntry {name: "STA", handler: CPU::sta, opcode: 0x9d, mode: AddressingModes::AbsoluteX, len: 3, cycles: 5},
    InstrEntry {name: "STA", handler: CPU::sta, opcode: 0x99, mode: AddressingModes::AbsoluteY, len: 3, cycles: 5},
    InstrEntry {name: "STA", handler: CPU::sta, opcode: 0x81, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry {name: "STA", handler: CPU::sta, opcode: 0x91, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 6},
    
    InstrEntry {name: "STX", handler: CPU::stx, opcode: 0x86, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "STX", handler: CPU::stx, opcode: 0x96, mode: AddressingModes::ZeroPageY, len: 2, cycles: 4},
    InstrEntry {name: "STX", handler: CPU::stx, opcode: 0x8e, mode: AddressingModes::Absolute, len: 3, cycles: 4},

    InstrEntry {name: "STY", handler: CPU::sty, opcode: 0x84, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry {name: "STY", handler: CPU::sty, opcode: 0x94, mode: AddressingModes::ZeroPageY, len: 2, cycles: 4},
    InstrEntry {name: "STY", handler: CPU::sty, opcode: 0x8c, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    
    InstrEntry {name: "TAX", handler: CPU::tax, opcode: 0xaa, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "TAY", handler: CPU::tay, opcode: 0xa8, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "TSX", handler: CPU::tsx, opcode: 0xba, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "TXS", handler: CPU::txs, opcode: 0x9a, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    
    InstrEntry {name: "TXA", handler: CPU::txa, opcode: 0x8a, mode: AddressingModes::Implicit, len: 1, cycles: 2},
    InstrEntry {name: "TYA", handler: CPU::tya, opcode: 0x98, mode: AddressingModes::Implicit, len: 1, cycles: 2},

    // ---------------
    // Illegal opcodes
    // ---------------

    InstrEntry { name: "ALR", handler: CPU::alr, opcode: 0x4b, mode: AddressingModes::Immediate, len: 2, cycles: 2},

    InstrEntry { name: "ANC", handler: CPU::anc, opcode: 0x0b, mode: AddressingModes::Immediate, len: 2, cycles: 2},
    InstrEntry { name: "ANC", handler: CPU::anc, opcode: 0x2b, mode: AddressingModes::Immediate, len: 2, cycles: 2},

    InstrEntry { name: "ANE", handler: CPU::ane, opcode: 0x8b, mode: AddressingModes::Immediate, len: 2, cycles: 2},

    InstrEntry { name: "ARR", handler: CPU::arr, opcode: 0x6b, mode: AddressingModes::Immediate, len: 2, cycles: 2},

    InstrEntry { name: "DCP", handler: CPU::dcp, opcode: 0xc7, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry { name: "DCP", handler: CPU::dcp, opcode: 0xd7, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry { name: "DCP", handler: CPU::dcp, opcode: 0xcf, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry { name: "DCP", handler: CPU::dcp, opcode: 0xdf, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    InstrEntry { name: "DCP", handler: CPU::dcp, opcode: 0xdb, mode: AddressingModes::AbsoluteY, len: 3, cycles: 7},
    InstrEntry { name: "DCP", handler: CPU::dcp, opcode: 0xc3, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 8},
    InstrEntry { name: "DCP", handler: CPU::dcp, opcode: 0xd3, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 8},
    
    InstrEntry { name: "ISC", handler: CPU::isc, opcode: 0xe7, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry { name: "ISC", handler: CPU::isc, opcode: 0xf7, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry { name: "ISC", handler: CPU::isc, opcode: 0xef, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry { name: "ISC", handler: CPU::isc, opcode: 0xff, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    InstrEntry { name: "ISC", handler: CPU::isc, opcode: 0xfb, mode: AddressingModes::AbsoluteY, len: 3, cycles: 7},
    InstrEntry { name: "ISC", handler: CPU::isc, opcode: 0xe3, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 8},
    InstrEntry { name: "ISC", handler: CPU::isc, opcode: 0xf3, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 8},

    InstrEntry { name: "LAS", handler: CPU::las, opcode: 0xbb, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},

    InstrEntry { name: "LAX", handler: CPU::lax, opcode: 0xa7, mode: AddressingModes::ZeroPage, len: 2, cycles: 3},
    InstrEntry { name: "LAX", handler: CPU::lax, opcode: 0xb7, mode: AddressingModes::ZeroPageY, len: 2, cycles: 4},
    InstrEntry { name: "LAX", handler: CPU::lax, opcode: 0xaf, mode: AddressingModes::Absolute, len: 3, cycles: 4},
    InstrEntry { name: "LAX", handler: CPU::lax, opcode: 0xbf, mode: AddressingModes::AbsoluteY, len: 3, cycles: 4},
    InstrEntry { name: "LAX", handler: CPU::lax, opcode: 0xa3, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 6},
    InstrEntry { name: "LAX", handler: CPU::lax, opcode: 0xb3, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 5},

    InstrEntry { name: "LXA", handler: CPU::lxa, opcode: 0xab, mode: AddressingModes::Immediate, len: 2, cycles: 2},

    InstrEntry { name: "LRA", handler: CPU::rla, opcode: 0x27, mode: AddressingModes::ZeroPage, len: 2, cycles: 5},
    InstrEntry { name: "LRA", handler: CPU::rla, opcode: 0x37, mode: AddressingModes::ZeroPageX, len: 2, cycles: 6},
    InstrEntry { name: "LRA", handler: CPU::rla, opcode: 0x2f, mode: AddressingModes::Absolute, len: 3, cycles: 6},
    InstrEntry { name: "LRA", handler: CPU::rla, opcode: 0x3f, mode: AddressingModes::AbsoluteX, len: 3, cycles: 7},
    InstrEntry { name: "LRA", handler: CPU::rla, opcode: 0x3b, mode: AddressingModes::AbsoluteY, len: 3, cycles: 7},
    InstrEntry { name: "LRA", handler: CPU::rla, opcode: 0x23, mode: AddressingModes::IndexedIndirect, len: 2, cycles: 8},
    InstrEntry { name: "LRA", handler: CPU::rla, opcode: 0x33, mode: AddressingModes::IndirectIndexed, len: 2, cycles: 8},

    //InstrEntry { name: "", handler: CPU::, opcode: 0x, mode: AddressingModes::, len: , cycles: },
];

pub fn get_instr(opcode: u8) -> Option<&'static InstrEntry> {
    INSTR_LIST.iter().find(|instr| instr.opcode == opcode)
}

impl CPU {
    fn adc(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        let orig_sign = self.a >> 7;
        let carry1: bool;
        let carry2: bool;
        (self.a, carry1) = self.a.overflowing_add(data);
        (self.a, carry2) = self.a.overflowing_add(self.flags.carry as u8);
        self.flags.carry = carry1 || carry2;
        self.flags.zero = self.a == 0;
        self.flags.overflow = (orig_sign == data >> 7) && (orig_sign != self.a >> 7);
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn and(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);
        
        self.a = self.a & data;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1
        };
    }

    fn asl(&mut self, mode: &AddressingModes) {
        if *mode == AddressingModes::Accumulator {
            self.flags.carry = self.a >> 7 == 1;
            self.a <<= 1;
            self.flags.zero = self.a == 0;
            self.flags.negative = self.a >> 7 == 1;
        }
        else {
            let addr = self.get_operand_addr(mode).0;
            self.flags.carry = self.read_mem(addr) >> 7 == 1;
            self.write_mem(addr, self.read_mem(addr) << 1);
            self.flags.zero = self.read_mem(addr) == 0;
            self.flags.negative = self.read_mem(addr) >> 7 == 1;
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
        let data = self.get_data(mode).0;
        self.flags.zero = self.a & data == 0;
        self.flags.negative = data >> 7 == 1;
        self.flags.overflow = (data >> 6) & 1 == 1;
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

    fn brk(&mut self, _mode: &AddressingModes) { // different sources say Different things about this instruction
        self.push16(self.pc + 2); // todo: this might be +1

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

        self.flags.carry = self.a >= data;
        self.flags.zero = self.a == data;
        self.flags.negative = self.a.overflowing_sub(data).1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn cpx(&mut self, mode: &AddressingModes) {
        let data = self.get_data(mode).0;

        self.flags.carry = self.x >= data;
        self.flags.zero = self.x == data;
        self.flags.negative = self.x.overflowing_sub(data).1;
    }
    
    fn cpy(&mut self, mode: &AddressingModes) {
        let data = self.get_data(mode).0;

        self.flags.carry = self.y >= data;
        self.flags.zero = self.y == data;
        self.flags.negative = self.y.overflowing_sub(data).1;
    }

    fn dec(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        self.write_mem(addr, self.read_mem(addr).wrapping_sub(1));

        self.flags.zero = self.read_mem(addr) == 0;
        self.flags.negative = self.read_mem(addr) >> 7 == 1;
    }

    fn dex(&mut self, _mode: &AddressingModes) {
        self.x = self.x.wrapping_sub(1);

        self.flags.zero = self.x == 0;
        self.flags.negative = self.x >> 7 == 1;
    }

    fn dey(&mut self, _mode: &AddressingModes) {
        self.y = self.y.wrapping_sub(1);

        self.flags.zero = self.y == 0;
        self.flags.negative = self.y >> 7 == 1;
    }

    fn eor(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.a ^= data;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn inc(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        self.write_mem(addr, self.read_mem(addr).wrapping_add(1));

        self.flags.zero = self.read_mem(addr) == 0;
        self.flags.negative = self.read_mem(addr) >> 7 == 1;
    }

    fn inx(&mut self, _mode: &AddressingModes) {
        self.x = self.x.wrapping_add(1);

        self.flags.zero = self.x == 0;
        self.flags.negative = self.x >> 7 == 1;
    }

    fn iny(&mut self, _mode: &AddressingModes) {
        self.y = self.y.wrapping_add(1);

        self.flags.zero = self.y == 0;
        self.flags.negative = self.y >> 7 == 1;
    }

    fn jmp(&mut self, mode: &AddressingModes) {
        self.pc = self.get_operand_addr(mode).0; 

        self.pc_autoincrement = false;
    }

    fn jsr(&mut self, _mode: &AddressingModes) {
        self.push16(self.pc + 2);

        self.pc = self.read_mem_u16(self.pc + 1); 

        self.pc_autoincrement = false;
    }

    fn lda(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.a = data;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn ldx(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.x = data;
        self.flags.zero = self.x == 0;
        self.flags.negative = self.x >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn ldy(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.y = data;
        self.flags.zero = self.y == 0;
        self.flags.negative = self.y >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn lsr(&mut self, mode: &AddressingModes) {
        if *mode == AddressingModes::Accumulator {
            self.flags.carry = self.a & 1 == 1;
            self.a >>= 1;
            self.flags.zero = self.a == 0;
            self.flags.negative = false;
        }
        else {
            let addr = self.get_operand_addr(mode).0;
            self.flags.carry = self.read_mem(addr) & 1 == 1;
            self.write_mem(addr, self.read_mem(addr) >> 1);
            self.flags.zero = self.read_mem(addr) == 0;
            self.flags.negative = false;
        }
    }

    fn nop(&mut self, _mode: &AddressingModes) {
    }

    fn ora(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.a |= data;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn pha(&mut self, _mode: &AddressingModes) {
        self.push(self.a);
    }

    fn php(&mut self, _mode: &AddressingModes) {
        self.push(super::Flags {
            carry: self.flags.carry,
            zero: self.flags.zero,
            interrupt_disable: self.flags.interrupt_disable,
            decimal_mode: self.flags.decimal_mode,
            breakfl: true,
            unused: true,
            overflow: self.flags.overflow,
            negative: self.flags.negative,
        }.get_u8());
    }

    fn pla(&mut self, _mode: &AddressingModes) {
        self.a = self.pop();
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;
    }

    fn plp(&mut self, _mode: &AddressingModes) {
        let data = self.pop();
        self.flags.set_u8(data);
        // breakfl and unused might be ignored ???
    }

    fn rol(&mut self, mode: &AddressingModes) {
        if *mode == AddressingModes::Accumulator {
            let bit0 = self.flags.carry;
            self.flags.carry = self.a >> 7 == 1;
            self.a <<= 1;
            if bit0 {
                self.a |= 0b00000001;
            }
            self.flags.zero = self.a == 0;
            self.flags.negative = self.a >> 7 == 1;
        }
        else {
            let addr = self.get_operand_addr(mode).0;
            let bit0 = self.flags.carry;
            let data = self.read_mem(addr);
            self.flags.carry = data >> 7 == 1;
            let mut res = data << 1;
            if bit0 {
                res |= 0b00000001;
            }
            self.write_mem(addr, res);
            self.flags.zero = data == 0;
            self.flags.negative = data >> 7 == 1;
        }
    }

    fn ror(&mut self, mode: &AddressingModes) {
        if *mode == AddressingModes::Accumulator {
            let bit7 = self.flags.carry;
            self.flags.carry = self.a & 1 == 1;
            self.a >>= 1;
            if bit7 {
                self.a |= 0b10000000;
            }
            self.flags.zero = self.a == 0;
            self.flags.negative = self.a >> 7 == 1;
        }
        else {
            let addr = self.get_operand_addr(mode).0;
            let bit7 = self.flags.carry;
            let data = self.read_mem(addr);
            self.flags.carry = data & 1 == 1;
            let mut res = data >> 1;
            if bit7 {
                res  |= 0b10000000;
            }
            self.write_mem(addr, res);
            self.flags.zero = data == 0;
            self.flags.negative = data >> 7 == 1;
        }
    }

    fn rti(&mut self, _mode: &AddressingModes) {
        let data = self.pop();
        self.flags.set_u8(data);
        // breakfl and unused might be ignored ???
        self.pc = self.pop16();

        self.pc_autoincrement = false; // todo: this might be wrong, the bug is probably somewhere else
    }

    fn rts(&mut self, _mode: &AddressingModes) {
        self.pc = self.pop16();
    }

    fn sbc(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);
        
        let orig_sign = self.a >> 7;
        let carry1: bool;
        let carry2: bool;
        (self.a, carry1) = self.a.overflowing_sub(data);
        (self.a, carry2) = self.a.overflowing_sub(self.flags.carry as u8);
        self.flags.carry = carry1 || carry2;
        self.flags.zero = self.a == 0;
        self.flags.overflow = (orig_sign == data >> 7) && (orig_sign != self.a >> 7);
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1
        };
    }

    fn sec(&mut self, _mode: &AddressingModes) {
        self.flags.carry = true;
    }

    fn sed(&mut self, _mode: &AddressingModes) {
        self.flags.decimal_mode = true;
    }

    fn sei(&mut self, _mode: &AddressingModes) {
        self.flags.interrupt_disable = true;
    }

    fn sta(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        self.write_mem(addr, self.a);
    }

    fn stx(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        self.write_mem(addr, self.x);
    }

    fn sty(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        self.write_mem(addr, self.y);
    }

    fn tax(&mut self, _mode: &AddressingModes) {
        self.x = self.a;

        self.flags.zero = self.x == 0;
        self.flags.negative = self.x >> 7 == 1;
    }

    fn tay(&mut self, _mode: &AddressingModes) {
        self.y = self.a;

        self.flags.zero = self.y == 0;
        self.flags.negative = self.y >> 7 == 1;
    }

    fn tsx(&mut self, _mode: &AddressingModes) {
        self.x = self.sp;

        self.flags.zero = self.x == 0;
        self.flags.negative = self.x >> 7 == 1;
    }

    fn txa(&mut self, _mode: &AddressingModes) {
        self.a = self.x;

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;
    }

    fn txs(&mut self, _mode: &AddressingModes) {
        self.sp = self.x;
    }

    fn tya(&mut self, _mode: &AddressingModes) {
        self.a = self.y;

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;
    }

    // ---------------
    // Illegal opcodes
    // ---------------

    fn alr(&mut self, mode: &AddressingModes) {
        self.a = self.a & self.get_data(mode).0;
        self.flags.carry = self.a & 1 == 1;
        self.a >>= 1;

        self.flags.zero = self.a == 0;
        self.flags.negative = false;
    }

    fn anc(&mut self, mode: &AddressingModes) {
        self.a = self.a & self.get_data(mode).0;

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;
        self.flags.carry = self.flags.negative;
    }

    fn ane(&mut self, mode: &AddressingModes) {
        self.a = (self.a | 0) & self.x & self.get_data(mode).0;

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;
    }

    fn arr(&mut self, mode: &AddressingModes) {
        self.a = self.a & self.get_data(mode).0;
        self.a = self.a.rotate_right(1); // different sites say different things, maybe here I have to use carry?

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;

        (self.flags.carry, self.flags.overflow) = match (self.a >> 5) & 3 {
            0 => (false, false),
            1 => (false, true),
            2 => (true, true),
            3 => (true, false),
            _ => unreachable!()
        }
    }

    fn dcp(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        let data = self.read_mem(addr).wrapping_sub(1);
        self.write_mem(addr, data);

        self.flags.carry = self.a >= data;
        self.flags.zero = self.a == data;
        self.flags.negative = self.a.overflowing_sub(data).1;
    }

    fn isc(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        let data = self.read_mem(addr).wrapping_add(1);
        self.write_mem(addr, data);

        let orig_sign = self.a >> 7;
        let carry1: bool;
        let carry2: bool;
        (self.a, carry1) = self.a.overflowing_sub(data);
        (self.a, carry2) = self.a.overflowing_sub(self.flags.carry as u8);
        self.flags.carry = carry1 || carry2;
        self.flags.zero = self.a == 0;
        self.flags.overflow = (orig_sign == data >> 7) && (orig_sign != self.a >> 7);
        self.flags.negative = self.a >> 7 == 1;
    }

    fn las(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.a = data & self.sp;
        self.x = self.a;
        self.sp = self.a;

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn lax(&mut self, mode: &AddressingModes) {
        let (data, page_crossed) = self.get_data(mode);

        self.a = data;
        self.x = self.a;

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;

        if page_crossed {
            self.cycles += 1;
        }
    }

    fn lxa(&mut self, mode: &AddressingModes) {
        self.a = (self.a | 0) & self.get_data(mode).0;
        self.x = self.a;

        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;
    }

    fn rla(&mut self, mode: &AddressingModes) {
        let addr = self.get_operand_addr(mode).0;
        let bit0 = self.flags.carry;
        let data = self.read_mem(addr);
        self.flags.carry = data >> 7 == 1;
        let mut res = data << 1;
        if bit0 {
            res |= 0b00000001;
        }
        self.write_mem(addr, res);
        self.a &= data;
        
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a >> 7 == 1;
    }

    fn rra(&mut self, mode: &AddressingModes) {
        todo!();
    }
}