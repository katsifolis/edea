use std::fmt::{Display, Formatter};
use std::error::Error;

use crate::interconnect::Interconnect;
pub struct Cpu {
    pc: u32,

    // General Purpose Registres
    regs: [u32;32],

    inter: Interconnect,
}


impl Cpu {

    pub fn new(inter: Interconnect) -> Cpu {
        let mut regs = [0xc0ffeeee; 32];

        regs[0] = 0;

        Cpu {
            pc: 0xBFC00000,
            inter: inter,
            regs: regs,
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

    fn load32 (&self, addr: u32) -> u32 {
        self.inter.load32(addr)
    }

    fn op_lui(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm();
        let t: u32 = instruction.t();

        let v = i << 16;

        self.set_reg(t, v);
    }

    fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.func() {
            0b001111 => self.op_lui(instruction),
            _ => panic!("Unhandled instruction {:x}", instruction.0),
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

    fn imm(self) -> u32 {
        let Instruction(op) = self;
        op & 0xffff
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let func = self.func();
        let rt = self.t();
        let imm = self.imm();
        write!(f, "Full Instruction: {:#b}\nFunction: {:#b}\nRegister target: {:#b}\nImmediate value: {:#b}", self.0, func, rt, imm)
    }
}