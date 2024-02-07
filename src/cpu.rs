use std::fmt::{Display, Formatter};

use crate::interconnect::Interconnect;
pub struct Cpu {
    pc: u32,

    // General Purpose Registres
    regs: [u32; 32],

    inter: Interconnect,
}

impl Cpu {
    pub fn new(inter: Interconnect) -> Cpu {
        let mut regs = [0xc0ffeeee; 32];

        regs[0] = 0;

        Cpu {
            pc: 0xBFC00000,
            inter,
            regs,
        }
    }

    fn reg(&self, idx: u32) -> u32 {
        self.regs[idx as usize]
    }

    fn set_reg(&mut self, idx: u32, val: u32) {
        self.regs[idx as usize] = val;
        self.regs[0] = 0;
    }

    pub fn run_next_instruction(&mut self) {
        let pc = self.pc;
        let instruction = Instruction(self.load32(pc));
        self.pc = pc.wrapping_add(4);
        self.decode_and_execute(instruction);

        println!("{}", instruction);
    }

    fn load32(&self, addr: u32) -> u32 {
        self.inter.load32(addr)
    }

    fn store32(&self, addr: u32, val: u32) {
        self.inter.store32(addr, val)
    }

    fn op_lui(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm();
        let t: u32 = instruction.t();
        let v = i << 16;
        self.set_reg(t, v);
    }

    fn op_ori(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm();
        let t: u32 = instruction.t();
        let s: u32 = instruction.s();

        let v = self.reg(s) | i;
        self.set_reg(t, v);
    }

    fn op_sw(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm_se();
        let t: u32 = instruction.t();
        let s: u32 = instruction.s();

        let addr = self.reg(s).wrapping_add(i);
        let val = self.reg(t);
        self.store32(addr, val);
    }

    fn op_sll(&mut self, instruction: Instruction) {
        let i: u32 = instruction.shift();
        let t: u32 = instruction.t();
        let d: u32 = instruction.d();

        let v = self.reg(t) << i;
        self.set_reg(d, v)
    }

    fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.func() {
            0b000000 => match instruction.subfunction() {
                0b000000 => self.op_sll(instruction),
                _ => panic!("Unhandled instruction {:X}", instruction.0),
            },
            0b001111 => self.op_lui(instruction),
            0b001101 => self.op_ori(instruction),
            0b101011 => self.op_sw(instruction),
            _ => panic!("Unhandled instruction {:X}", instruction.0),
        }
    }
}

#[derive(Clone, Copy)]
struct Instruction(u32);

impl Instruction {
    fn func(self) -> u32 {
        let Instruction(op) = self;
        op >> 26
    }

    fn t(self) -> u32 {
        let Instruction(op) = self;
        (op >> 16) & 0x1f
    }

    fn s(self) -> u32 {
        let Instruction(op) = self;
        (op >> 21) & 0x1f
    }

    fn d(self) -> u32 {
        let Instruction(op) = self;
        (op >> 11) & 0x1f
    }

    fn imm(self) -> u32 {
        let Instruction(op) = self;
        op & 0xffff
    }

    fn imm_se(self) -> u32 {
        let Instruction(op) = self;
        let v = (op & 0xffff) as i16;
        v as u32
    }

    fn subfunction(self) -> u32 {
        let Instruction(op) = self;
        op & 0x3f
    }

    fn shift(self) -> u32 {
        let Instruction(op) = self;
        (op >> 6) & 0x1f
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let func = self.func();
        let rt = self.t();
        let rs = self.s();
        let imm = self.imm();
        write!(f, "Full Instruction: {:#b}\nFunction: {:#b}\nRegister target: {:#b}\nRegister source: {:#b}\nImmediate value: {:#b}", self.0, func, rt, rs, imm)
    }
}
