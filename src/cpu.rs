use std::fmt::{Display, Formatter};

use crate::interconnect::Interconnect;
pub struct Cpu {
    pc: u32,

    /// Cop 0 register 12: Status Register
    sr: u32,

    /// HI register for div remainder and mul high result
    hi: u32,

    /// Low register for div quotient and mul low result
    lo: u32,

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
            hi: 0xcafecafe,
            lo: 0xcafecafe,
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

    fn load8(&self, addr: u32) -> u8 {
        self.inter.load8(addr)
    }

    /// Store a word into memory
    fn store32(&mut self, addr: u32, val: u32) {
        self.inter.store32(addr, val)
    }

    /// Store a half-word into memory
    fn store16(&mut self, addr: u32, val: u16) {
        self.inter.store16(addr, val)
    }

    /// Store a byte into memory
    fn store8(&mut self, addr: u32, val: u8) {
        self.inter.store8(addr, val)
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

    fn op_andi(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm();
        let t: RegisterIndex = instruction.t();
        let s: RegisterIndex = instruction.s();

        let v = self.reg(s) & i;
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
        self.set_reg(d, v);
    }

    fn op_sra(&mut self, instruction: Instruction) {
        let i: u32 = instruction.shift();
        let t: RegisterIndex = instruction.t();
        let d: RegisterIndex = instruction.d();

        let v = (self.reg(t) as i32) >> i;

        self.set_reg(d, v as u32);
    }

    fn op_srl(&mut self, instruction: Instruction) {
        let i: u32 = instruction.shift();
        let t: RegisterIndex = instruction.t();
        let d: RegisterIndex = instruction.d();

        let v = self.reg(t) >> i;

        self.set_reg(d, v);
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

    fn op_and(&mut self, instruction: Instruction) {
        let d: RegisterIndex = instruction.d();
        let s: RegisterIndex = instruction.s();
        let t: RegisterIndex = instruction.t();
        let v = self.reg(s) & self.reg(t);
        self.set_reg(d, v);
    }

    fn op_j(&mut self, instruction: Instruction) {
        let i: u32 = instruction.imm_jump();
        self.pc = (self.pc & 0xF0000000) | (i << 2);
    }

    fn op_cop0(&mut self, instruction: Instruction) {
        match instruction.cop_opcode() {
            0b00100 => self.op_mtc0(instruction),
            0b00000 => self.op_mfc0(instruction),
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

    fn op_mfc0(&mut self, instruction: Instruction) {
        let cpu_r = instruction.t();
        let cop_r = instruction.d().0;

        let v = match cop_r {
            12 => self.sr,
            13 => panic!("Unhandled read from CAUSE register"),
            _ => panic!("Unhandled read from cop0{}", cop_r),
        };

        self.load = (cpu_r, v);
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

    fn op_beq(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        if self.reg(s) == self.reg(t) {
            self.branch(i)
        }
    }

    fn op_bgtz(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();

        let v = self.reg(s) as i32;

        if v > 0 {
            self.branch(i);
        }
    }

    fn op_blez(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();

        let v = self.reg(s) as i32;

        if v <= 0 {
            self.branch(i);
        }
    }

    fn op_bxx(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let instruction = instruction.0;
        let is_bgez = (instruction >> 16) & 1;
        let is_link = (instruction >> 20) & 1 != 0;
        let v = self.reg(s) as i32;
        let test = (v < 0) as u32;
        let test = test ^ is_bgez;

        /*
           Basically just a xor table

           First case:
               v -> 123, test = 0, is_bgez = 1
               test = 1
           Second example:
               v -> -1, test = 1, is_bgez = 1
               test = 0
           Third example:
               v -> 123, test = 0, is_bgez = 0
               test = 0
           Fourth example:
               v -> -1, test = 1, is_bgez = 0
               test = 1
        */
        if test != 0 {
            if is_link {
                let ra = self.pc;

                self.set_reg(RegisterIndex(31), ra);
            }
            self.branch(i);
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

    fn op_add(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let s = self.reg(s) as i32;
        let t = self.reg(t) as i32;

        let v = match s.checked_add(t) {
            Some(v) => v as u32,
            None => panic!("ADD Overflow"),
        };

        self.set_reg(d, v);
    }

    fn op_subu(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s).wrapping_sub(self.reg(t));

        self.set_reg(d, v);
    }

    fn op_div(&mut self, instruction: Instruction) {
        let s = instruction.s();
        let t = instruction.t();

        let n = self.reg(s) as i32;
        let d = self.reg(t) as i32;

        if n == 0 {
            self.hi = n as u32;
            if n >= 0 {
                self.lo = 0xffff_ffff;
            } else {
                self.lo = 1;
            }
        } else if n as u32 == 0x8000_0000 && d == -1 {
            self.hi = 0;
            self.lo = 0x80000000;
        } else {
            self.hi = (n % d) as u32;
            self.lo = (n / d) as u32;
        }
    }

    fn op_divu(&mut self, instruction: Instruction) {
        let s = instruction.s();
        let t = instruction.t();

        let n = self.reg(s);
        let d = self.reg(t);

        if d == 0 {
            self.hi = n;
            self.lo = 0xffffffff;
        } else {
            self.hi = n % d;
            self.lo = n / d;
        }
    }

    fn op_mflo(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let lo = self.lo;
        self.set_reg(d, lo);
    }

    fn op_mfhi(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let hi = self.hi;
        self.set_reg(d, hi);
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

    fn op_lb(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("Ignoring load while cache is isolated");
            return;
        }

        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load8(addr) as i8;

        self.load = (t, v as u32);
    }

    fn op_lbu(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load8(addr);

        self.load = (t, v as u32);
    }

    fn op_slt(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let s = self.reg(s) as i32;
        let t = self.reg(t) as i32;

        let v = s < t;
        self.set_reg(d, v as u32);
    }

    fn op_sltu(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s) < self.reg(t);
        self.set_reg(d, v as u32);
    }

    fn op_slti(&mut self, instruction: Instruction) {
        let i = instruction.imm_se() as i32;
        let s = instruction.s();
        let t = instruction.t();

        let v = (self.reg(s) as i32) < i;

        self.set_reg(t, v as u32);
    }

    fn op_sltiu(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s) < i;

        self.set_reg(t, v as u32);
    }

    fn op_addu(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s).wrapping_add(self.reg(t));

        self.set_reg(d, v);
    }

    fn op_sh(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("Ignoring store while cache is isolated");
            return;
        }

        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);
        let val = self.reg(t);
        self.store16(addr, val as u16);
    }

    /// Jump and link
    fn op_jal(&mut self, instruction: Instruction) {
        let ra = self.pc;
        // Store return address to $ra
        self.set_reg(RegisterIndex(31), ra);
        self.op_j(instruction);
    }

    /// Jump register
    fn op_jr(&mut self, instruction: Instruction) {
        let s = instruction.s();
        self.pc = self.reg(s);
    }

    /// Jump and link register
    fn op_jalr(&mut self, instruction: Instruction) {
        let s = instruction.s();
        let d = instruction.d();
        let ra = self.pc;

        self.set_reg(d, ra);

        self.pc = self.reg(s);
    }

    fn op_sb(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("Ignoring store while cache is isolated");
            return;
        }

        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);
        let val = self.reg(t);
        self.store8(addr, val as u8);
    }

    fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.func() {
            0b000000 => match instruction.subfunction() {
                0b000000 => self.op_sll(instruction),
                0b100101 => self.op_or(instruction),
                0b101011 => self.op_sltu(instruction),
                0b101010 => self.op_slt(instruction),
                0b100001 => self.op_addu(instruction),
                0b001000 => self.op_jr(instruction),
                0b001001 => self.op_jalr(instruction),
                0b100100 => self.op_and(instruction),
                0b100000 => self.op_add(instruction),
                0b100011 => self.op_subu(instruction),
                0b000011 => self.op_sra(instruction),
                0b000010 => self.op_srl(instruction),
                0b011010 => self.op_div(instruction),
                0b010010 => self.op_mflo(instruction),
                0b010000 => self.op_mfhi(instruction),
                0b011011 => self.op_divu(instruction),
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
            0b000011 => self.op_jal(instruction),
            0b010000 => self.op_cop0(instruction),
            0b000101 => self.op_bne(instruction),
            0b000100 => self.op_beq(instruction),
            0b001000 => self.op_addi(instruction),
            0b100011 => self.op_lw(instruction),
            0b101001 => self.op_sh(instruction),
            0b001100 => self.op_andi(instruction),
            0b101000 => self.op_sb(instruction),
            0b100000 => self.op_lb(instruction),
            0b100100 => self.op_lbu(instruction),
            0b000111 => self.op_bgtz(instruction),
            0b000110 => self.op_blez(instruction),
            0b000001 => self.op_bxx(instruction),
            0b001010 => self.op_slti(instruction),
            0b001011 => self.op_sltiu(instruction),
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
