use std::fmt::{Display, Formatter};

use crate::interconnect::Interconnect;
pub struct Cpu {
    pc: u32,

    /// Cop 0 register 12: Status Register
    sr: u32,

    /// Next Instruction To be executed
    /// simulating the way ps1 works (branch delay slot)
    next_instruction: Instruction,

    /// 2nd set of regs used to emulate the load delay slot
    /// accurately. They contain the output of the current
    /// instruction
    out_regs: [u32; 32],

    /// Load initiated by the current instruction
    load: (RegisterIndex, u32),

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
            out_regs: regs,
            load: (RegisterIndex(0), 0),
            regs,
        }
    }

    fn reg(&self, idx: RegisterIndex) -> u32 {
        self.regs[idx.0 as usize]
    }

    fn set_reg(&mut self, idx: RegisterIndex, val: u32) {
        self.out_regs[idx.0 as usize] = val;
        self.out_regs[0] = 0;
    }

    pub fn run_next_instruction(&mut self) {
        let pc = self.pc;
        let instruction = self.next_instruction;
        self.next_instruction = Instruction(self.load32(pc));
        self.pc = pc.wrapping_add(4);

        // Execute the pending load (if any, otherwise it will load
        // $zero which is NOP). `set_reg` works only on `out_regs` so
        // this operation won't be visible by the next instruction
        let (reg, val) = self.load;
        self.set_reg(reg, val);

        // We reset the load to target register 0 for the next instruction
        self.load = (RegisterIndex(0), 0);
        self.decode_and_execute(instruction);

        // Copy the output registers as input for the next instruction
        self.regs = self.out_regs;

        //println!("{}", instruction);
    }

    fn load32(&self, addr: u32) -> u32 {
        self.inter.load32(addr)
    }

    /// Store
    fn store32(&mut self, addr: u32, val: u32) {
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
            3 | 5 | 6 | 7 | 9 | 11 => {
                if v != 0 {
                    panic!("Unhandled write to cop0r{}", cop_r)
                }
            }
            12 => self.sr = v,
            13 => {
                if v != 0 {
                    panic!("Unhandled write to CAUSE register.")
                }
            }
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

    fn op_addi(&mut self, instruction: Instruction) {
        let i = instruction.imm_se() as i32;
        let s = instruction.s();
        let t = instruction.t();

        let s = self.reg(s) as i32;

        let v = match s.checked_add(i) {
            Some(v) => v as u32,
            None => panic!("ADDI Overflow"),
        };

        self.set_reg(t, v);
    }

    fn op_lw(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("Ignoring load while cache is isolated");
            return;
        }

        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load32(addr);

        self.load = (t, v);
    }

    fn op_sltu(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s) < self.reg(t);
        self.set_reg(d, v as u32);
    }

    fn op_addu(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s).wrapping_add(self.reg(t));

        self.set_reg(d, v);
    }

    fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.func() {
            0b000000 => match instruction.subfunction() {
                0b000000 => self.op_sll(instruction),
                0b100101 => self.op_or(instruction),
                0b101011 => self.op_sltu(instruction),
                0b100001 => self.op_addu(instruction),
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
            0b001000 => self.op_addi(instruction),
            0b100011 => self.op_lw(instruction),
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
