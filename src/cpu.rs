use std::fmt::{Display, Formatter};

use crate::interconnect::Interconnect;
pub struct Cpu {
    pc: u32,

    /// Cop 0 register 12: Status Register
    sr: u32,

    /// Next Instruction To be executed
    /// simulating the way ps1 works (branch delay slot)
    next_instruction: Instruction,

    // General Purpose Registres
    regs: [u32; 32],

    inter: Interconnect,
}

#[derive(Copy, Clone)]
struct RegisterIndex(u32);

impl Cpu {
    pub fn new(inter: Interconnect) -> Cpu {
        let mut regs = [0xdeadbeef; 32];

        regs[0] = 0;

        Cpu {
            pc: 0xBFC00000,
            sr: 0,
            next_instruction: Instruction(0x0),
            inter,
            regs,
        }
    }

    fn reg(&self, idx: RegisterIndex) -> u32 {
        self.regs[idx.0 as usize]
    }

    fn set_reg(&mut self, idx: RegisterIndex, val: u32) {
        self.regs[idx.0 as usize] = val;
        self.regs[0] = 0;
    }

    pub fn run_next_instruction(&mut self) {
        let pc = self.pc;
        let instruction = self.next_instruction;
        self.next_instruction = Instruction(self.load32(pc));
        self.pc = pc.wrapping_add(4);
        self.decode_and_execute(instruction);

        println!("{}", instruction);
    }

    fn load32(&self, addr: u32) -> u32 {
        self.inter.load32(addr)
    }

    /// Store
    fn store32(&self, addr: u32, val: u32) {
        self.inter.store32(addr, val)
    }

    fn op_lui(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm();
        let t: RegisterIndex = instruction.t();
        let v = i << 16;
        self.set_reg(t, v);
    }

    fn op_ori(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm();
        let t: RegisterIndex = instruction.t();
        let s: RegisterIndex = instruction.s();

        let v = self.reg(s) | i;
        self.set_reg(t, v);
    }

    fn op_sw(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("Ignoring store while cache is isolated");
            return;
        }

        let i: u32 = instruction.imm_se();
        let t: RegisterIndex = instruction.t();
        let s: RegisterIndex = instruction.s();

        let addr = self.reg(s).wrapping_add(i);
        let val = self.reg(t);
        self.store32(addr, val);
    }

    fn op_sll(&mut self, instruction: Instruction) {
        let i: u32 = instruction.shift();
        let t: RegisterIndex = instruction.t();
        let d: RegisterIndex = instruction.d();

        let v = self.reg(t) << i;
        self.set_reg(d, v)
    }

    fn op_addiu(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm_se();
        let t: RegisterIndex = instruction.t();
        let s: RegisterIndex = instruction.s();
        let v = self.reg(s).wrapping_add(i);

        self.set_reg(t, v);
    }

    fn op_or(&mut self, instruction: Instruction) {
        let d: RegisterIndex = instruction.d();
        let s: RegisterIndex = instruction.s();
        let t: RegisterIndex = instruction.t();
        let v = self.reg(s) | self.reg(t);
        self.set_reg(d, v);
    }

    fn op_j(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm_jump();
        self.pc = (self.pc & 0xF0000000) | (i << 2);
    }

    fn op_cop0(&mut self, instruction: Instruction) {
        match instruction.cop_opcode() {
            0b00100 => self.op_mtc0(instruction),
            _ => panic!("Unhandled cop0 instruction {}", instruction),
        }
    }

    /// Move to CoProcessor 0
    fn op_mtc0(&mut self, instruction: Instruction) {
        let cpu_r = instruction.t();
        let cop_r = instruction.d().0;

        let v = self.reg(cpu_r);

        match cop_r {
            12 => self.sr = v,
            n => panic!("Unhandled cop0 register: {:08X}", n),
        }
    }

    /// Branch to immediate value `offset`.
    fn branch(&mut self, offset: u32) {
        // Offset immediates are always shifted two places to the right
        // since `PC` addresses have to be aligned on 32bits at all time.
        let offset = offset << 2;

        let mut pc = self.pc;

        pc = pc.wrapping_add(offset);

        pc = pc.wrapping_sub(4);

        self.pc = pc;
    }

    fn op_bne(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        if self.reg(s) != self.reg(t) {
            self.branch(i)
        }
    }

    fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.func() {
            0b000000 => match instruction.subfunction() {
                0b000000 => self.op_sll(instruction),
                0b100101 => self.op_or(instruction),
                _ => panic!(
                    "Unhandled instruction {:08X} at PC -> {:04X}",
                    instruction.0, self.pc
                ),
            },
            0b001111 => self.op_lui(instruction),
            0b001101 => self.op_ori(instruction),
            0b101011 => self.op_sw(instruction),
            0b001001 => self.op_addiu(instruction),
            0b000010 => self.op_j(instruction),
            0b010000 => self.op_cop0(instruction),
            0b000101 => self.op_bne(instruction),
            _ => panic!(
                "Unhandled instruction {:08X} at PC -> {:04X}",
                instruction.0, self.pc
            ),
        }
    }
}

#[derive(Clone, Copy)]
struct Instruction(u32);

impl Instruction {
    fn func(self) -> u32 {
        self.0 >> 26
    }

    fn t(self) -> RegisterIndex {
        RegisterIndex((self.0 >> 16) & 0x1f)
    }

    fn s(self) -> RegisterIndex {
        RegisterIndex((self.0 >> 21) & 0x1f)
    }

    fn d(self) -> RegisterIndex {
        RegisterIndex((self.0 >> 11) & 0x1f)
    }

    fn imm(self) -> u32 {
        self.0 & 0xffff
    }

    fn imm_se(self) -> u32 {
        let v = (self.0 & 0xffff) as i16;
        v as u32
    }

    fn subfunction(self) -> u32 {
        self.0 & 0x3f
    }

    fn shift(self) -> u32 {
        (self.0 >> 6) & 0x1f
    }

    fn imm_jump(self) -> u32 {
        self.0 & 0x3ffffff
    }

    fn cop_opcode(self) -> u32 {
        (self.0 >> 21) & 0x1f
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let func = self.func();
        let rt: RegisterIndex = self.t();
        let rs: RegisterIndex = self.s();
        let imm = self.imm();
        write!(f, "Full Instruction: {:#b}\nFunction: {:#b}\nRegister target: {:#b}\nRegister source: {:#b}\nImmediate value: {:#b}", self.0, func, rt.0, rs.0, imm)
    }
}
