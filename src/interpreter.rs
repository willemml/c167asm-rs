use std::num::Wrapping;

use crate::Error;
use crate::Result;
use crate::addressing::Address;
use crate::instructions::*;
use crate::registers;
use crate::registers::SpecialFunctionRegister;

/// To identify "functions" follow jmp and call instructions, save IC, CP, and CSP into a stack
/// when end reached (or infinite loop detected) pop latest from stack, set registers back, and skip the jump
/// (this is only for conditional jumps/calls, otherwise don't bother going back)
///
/// Locate checksums via OBD code calls maybe?
///
/// Detect function signatures, upon call save recently modified (within last call/jump?) registers
/// and then check which ones are read before return. Also see which memory locations are looked at (constants/maps).
///
/// Might be able to ignore arithmetic ops and similar eventually?

macro_rules! mk_write_fn {
    ($type:ident, $t:ty) => {
        // TODO: allow for EXT addressing overrides
        // TODO: split into separate functions for bytewise and wordwise addresses
        pub fn ${concat(write_address_,$type)}<const N: u16>(&mut self, address: Address, val: $t) -> Result {
            match address {
                Address::GPR(a) => self.${concat(write_gpr_, $type)}(a, val),
                Address::Indirect(a) => self.${concat(write_dpp_addressed_, $type)}(self.read_gpr_word(a), val),
                Address::IndirectIncr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) + Wrapping(N)).0);

                    self.${concat(write_dpp_addressed_, $type)}(gpr, val)
                }
                Address::IndirectDecr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) - Wrapping(N)).0);

                    self.${concat(write_dpp_addressed_, $type)}(gpr, val)
                }
                Address::Mem(a) => self.${concat(write_dpp_addressed_, $type)}(a, val),
                Address::Reg(a) => {
                    if 0xF0 & a == 0xF0 {
                        self.${concat(write_gpr_, $type)}(a, val)
                    } else {
                        // TODO: handle ESFR
                        self.${concat(write_, $type)}(registers::sfr_addr_from_byte(a).unwrap_or(0), val)
                    }
                }
                Address::Bitaddr(addr, bit) => {
                    let word = self.${concat(read_, $type)}(addr as usize) & (0xFF ^ (1 << bit));

                    self.${concat(write_, $type)}(addr as usize, word | ((val & 1) << bit));
                }
                Address::Indirect16(a, c) => {
                    self.${concat(write_dpp_addressed_, $type)}(self.read_gpr_word(a) + c, val);
                }
                _ => return Err(Error::NotWriteable),
            }
            Ok(())
        }
    };
}
macro_rules! mk_read_fn {
    ($type:ident, $t:ty) => {
        // TODO: allow for EXT addressing overrides
        // TODO: split into separate functions for bytewise and wordwise addresses
        pub fn ${concat(read_address_, $type)}<const N: u16>(&mut self, address: Address) -> Result<$t, Error> {
            Ok(match address {
                Address::GPR(a) => self.${concat(read_gpr_, $type)}(a),
                Address::Indirect(a) => self.${concat(read_dpp_addressed_, $type)}(self.read_gpr_word(a)),
                Address::IndirectIncr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) + Wrapping(N)).0);
                    self.${concat(read_dpp_addressed_, $type)}(gpr)
                }
                Address::IndirectDecr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) - Wrapping(N)).0);

                    self.${concat(read_dpp_addressed_, $type)}(gpr)
                }
                Address::Data16(a) => a as $t,
                Address::Data8(a) => a as $t,
                Address::Data4(a) => a as $t,
                Address::Data3(a) => a as $t,
                Address::Mem(a) => self.${concat(read_dpp_addressed_, $type)}(a),
                Address::Reg(a) => {
                    if 0xF0 & a == 0xF0 {
                        self.${concat(read_gpr_, $type)}(a)
                    } else {
                        // TODO: handle ESFR
                        self.${concat(read_, $type)}(registers::sfr_addr_from_byte(a).unwrap_or(0))
                    }
                }
                Address::Special(s) => {
                    let mode = s >> 3;

                    if mode == 0 {
                        s as $t
                    } else {
                        let gpr = s & 0b11;
                        let v = self.read_gpr_word(gpr);
                        if (s >> 2) & 0b01 == 1 {
                            self.write_gpr_word(gpr, v + N);
                        }
                        self.${concat(read_dpp_addressed_, $type)}(v)
                    }
                }
                Address::Bitaddr(addr, bit) => (self.${concat(read_, $type)}(addr as usize) >> bit) & 1,
                Address::Indirect16(a, c) => {
                    self.${concat(read_dpp_addressed_, $type)}(self.read_gpr_word(a) + c)
                }
                _ => return Err(Error::NotReadable),
            })
        }
    };
}

pub struct Interpreter {
    pub memory: Vec<u8>,
    instruction_pointer: u16,
    flags: Flags,
}

#[derive(Copy, Clone, Debug, Default)]
struct Flags {
    /// Set if the value of op2 represents the lowest possible negative
    /// number. Cleared otherwise. Used to signal the end of a table.
    e: bool,
    /// Set if result equals zero. Cleared otherwise.
    z: bool,
    /// Set if an arithmetic overflow occurred, i.e. the result cannot be
    /// represented in the specified data type. Cleared otherwise.
    v: bool,
    /// Set if a carry is generated from the most significant bit of the
    /// specified data type. Cleared otherwise.
    c: bool,
    /// Set if the most significant bit of the result is set. Cleared
    /// otherwise.
    n: bool,
}

macro_rules! bitlogic {
    (bit: $({$op:tt})? ($a:ident, $b:ident) on $self:ident) => {
        let av = $self.read_bit($a)?;
        let bv = $self.read_bit($b)?;

        $self.flags.e = false;
        $self.flags.z = !(av || bv);
        $self.flags.v = av || bv;
        $self.flags.c = av && bv;
        $self.flags.n = av ^ bv;

        $($self.write_bit($a, av $op bv)?;)?
    };
    (byte: $op:tt ($a:ident, $b:ident) on $self:ident) => {
        let av = $self.read_address_byte::<1>($a)?;
        let bv = $self.read_address_byte::<1>($b)?;
        let n = av $op bv;

        $self.flags.v = false;
        $self.flags.c = false;
        $self.flags.e = bv as i8 == i8::MIN;
        $self.flags.z = bv == 0;
        $self.flags.n = n >> 7 == 1;

        $self.write_address_byte::<0>($a, n)?;
    };
    (word: $op:tt ($a:ident, $b:ident) on $self:ident) => {
        let av = $self.read_address_word::<2>($a)?;
        let bv = $self.read_address_word::<2>($b)?;
        let n = av $op bv;

        $self.flags.v = false;
        $self.flags.c = false;
        $self.flags.e = bv as i16 == i16::MIN;
        $self.flags.z = bv == 0;
        $self.flags.n = n >> 15 == 1;

        $self.write_address_word::<0>($a, n)?;
    };
}

macro_rules! arithmetic {
    (word:$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $self:ident, ($c1:ident, $val:ident) => $code:block) => {
        let a_initial = $self.read_address_word::<2>($a)?;
        let b_val = $self.read_address_word::<2>($b)?;
        #[allow(unused_mut)]
        let (mut $val, $c1) = a_initial.$fn(b_val);
        $self.flags.c = $code;
        $self.flags.v = ((a_initial ^ b_val) & 0x8000 == 0) && ((a_initial ^ $val) & 0x8000 != 0);
        $self.flags.z = $val == 0;
        $self.flags.n = $val >> 15 == 1;
        $self.flags.e = b_val as i16 == i16::MIN;
        $(${ignore($nw)} $self.write_address_word::<0>($a, $val)?;)?
    };
    (nc:$t:ident$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $self:ident) => {
        arithmetic!($t:$(($nw))? $fn($a,$b) with $self, (c1, val) => {c1});
    };
    (c:$t:ident$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $self:ident) => {
        arithmetic!($t:$(($nw))? $fn($a,$b) with $self, (c1, val) => {
            if $self.flags.c {
                let (valc, c2) = val.$fn(1);
                val = valc;
                c1 || c2
            } else { c1 }
        });
    };
    (byte:$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $self:ident, ($c1:ident, $val:ident) => $code:block) => {
        let a_initial = $self.read_address_byte::<1>($a)?;
        let b_val = $self.read_address_byte::<1>($b)?;
        #[allow(unused_mut)]
        let (mut $val, $c1) = a_initial.$fn(b_val);
        $self.flags.c = $code;
        $self.flags.v = ((a_initial ^ b_val) & 0x80 == 0) && ((a_initial ^ $val) & 0x80 != 0);
        $self.flags.z = $val == 0;
        $self.flags.n = $val >> 7 == 1;
        $self.flags.e = b_val as i8 == i8::MIN;
        $(${ignore($nw)} $self.write_address_byte::<0>($a, $val)?;)?
    };
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            memory: vec![0u8; 16_000_000],
            instruction_pointer: 0,
            flags: Flags::default(),
        }
    }

    pub fn execute(&mut self) -> Result {
        loop {
            let ip = self.get_ip();
            let instr = {
                let mut slice = &self.memory[ip..ip + 4];
                Instruction::read(&mut slice)?
            };
            let op = Operation::from(instr);

            println!("{:04x}: {:02x?}", self.instruction_pointer, op);
            match op {
                Operation::Add(a, b) => {
                    arithmetic!(nc:word(w) overflowing_add(a, b) with self);
                }
                Operation::AddC(a, b) => {
                    arithmetic!(c:word(w) overflowing_add(a, b) with self);
                }
                Operation::Sub(a, b) => {
                    arithmetic!(nc:word(w) overflowing_sub(a, b) with self);
                }
                Operation::SubC(a, b) => {
                    arithmetic!(c:word(w) overflowing_sub(a, b) with self);
                }
                Operation::AddB(a, b) => {
                    arithmetic!(nc:byte(w) overflowing_add(a, b) with self);
                }
                Operation::AddCB(a, b) => {
                    arithmetic!(c:byte(w) overflowing_add(a, b) with self);
                }
                Operation::SubB(a, b) => {
                    arithmetic!(nc:byte(w) overflowing_sub(a, b) with self);
                }
                Operation::SubCB(a, b) => {
                    arithmetic!(c:byte(w) overflowing_sub(a, b) with self);
                }

                Operation::Cmp(a, b) => {
                    arithmetic!(nc:word overflowing_sub(a, b) with self);
                }
                Operation::CmpB(a, b) => {
                    arithmetic!(nc:byte overflowing_sub(a, b) with self);
                }

                Operation::Or(a, b) => {
                    bitlogic!(word: | (a, b) on self);
                }
                Operation::OrB(a, b) => {
                    bitlogic!(byte: | (a, b) on self);
                }
                Operation::Xor(a, b) => {
                    bitlogic!(word: ^ (a, b) on self);
                }
                Operation::XorB(a, b) => {
                    bitlogic!(byte: ^ (a, b) on self);
                }
                Operation::And(a, b) => {
                    bitlogic!(word: & (a, b) on self);
                }
                Operation::AndB(a, b) => {
                    bitlogic!(byte: & (a, b) on self);
                }

                Operation::Mov(a, b) => {
                    let w = self.read_address_word::<2>(b)?;
                    self.flags.e = w as i16 == i16::MIN;
                    self.flags.z = w == 0;
                    self.flags.n = w >> 15 == 1;
                    self.write_address_word::<2>(a, w)?
                }
                Operation::MovB(a, b) => {
                    let b = self.read_address_byte::<1>(b)?;
                    self.flags.e = b as i8 == i8::MIN;
                    self.flags.z = b == 0;
                    self.flags.n = b >> 7 == 1;
                    self.write_address_byte::<1>(a, b)?
                }
                Operation::MovBZ(a, b) => {
                    let b = self.read_address_byte::<0>(b)?;
                    self.flags.e = false;
                    self.flags.z = b == 0;
                    self.flags.n = false;
                    self.write_address_word::<0>(a, u16::from_le_bytes([b, 0]))?
                }
                Operation::MovBS(a, b) => {
                    let b = self.read_address_byte::<0>(b)?;
                    let h = if b >> 7 == 1 { 0xFF } else { 0x00 };
                    self.flags.e = false;
                    self.flags.z = b == 0;
                    self.flags.n = b >> 7 == 1;
                    self.write_address_word::<0>(a, u16::from_le_bytes([b, h]))?
                }

                Operation::Band(a, b) => {
                    bitlogic!(bit: {&&} (a, b) on self);
                }
                Operation::Bor(a, b) => {
                    bitlogic!(bit: {||} (a, b) on self);
                }
                Operation::Bxor(a, b) => {
                    bitlogic!(bit: {^} (a, b) on self);
                }
                Operation::Bcmp(a, b) => {
                    bitlogic!(bit: (a, b) on self);
                }
                Operation::Bclr(bitaddr) => {
                    self.write_bit(bitaddr.into(), false)?;
                }
                Operation::Bset(bitaddr) => {
                    self.write_bit(bitaddr.into(), true)?;
                }
                Operation::Bmov(a, b) => {
                    let bv = self.read_bit(b)?;

                    self.flags.e = false;
                    self.flags.z = !bv;
                    self.flags.v = false;
                    self.flags.c = false;
                    self.flags.n = bv;

                    self.write_bit(a, bv)?;
                }
                Operation::BmovN(a, b) => {
                    let bv = self.read_bit(b)?;

                    self.flags.e = false;
                    self.flags.z = !bv;
                    self.flags.v = false;
                    self.flags.c = false;
                    self.flags.n = bv;

                    self.write_bit(a, !bv)?;
                }
                Operation::BFLDL(a, b, c) => todo!(),
                Operation::BFLDH(a, b, c) => todo!(),

                Operation::DISWDT() => todo!(),
                Operation::SRVWDT() => todo!(),

                Operation::EINIT() => {}
                Operation::IDLE() => {}
                Operation::NOP() => {}
                Operation::PWRDN() => return Ok(()),
                Operation::SRST() => todo!(),

                Operation::JB(a, b) => todo!(),
                Operation::JBC(a, b) => todo!(),
                Operation::JNB(a, b) => todo!(),
                Operation::JNBS(a, b) => todo!(),
                Operation::JmpR(condition_code, _) => todo!(),
                Operation::JmpA(a, b) => todo!(),
                Operation::JmpI(a, b) => todo!(),
                Operation::JmpS(a, b) => todo!(),

                Operation::RET() => todo!(),
                Operation::RETI() => todo!(),
                Operation::RETS() => todo!(),
                Operation::RETP(a) => todo!(),

                Operation::CallA(a, b) => todo!(),
                Operation::CallI(a, b) => todo!(),
                Operation::CallR(a) => todo!(),

                Operation::AtEx(a) => todo!(),
                Operation::CPLB(a) => todo!(),
                Operation::CPL(a) => todo!(),
                Operation::Div(a) => todo!(),
                Operation::Divl(a) => todo!(),
                Operation::Divlu(a) => todo!(),
                Operation::Divu(a) => todo!(),
                Operation::EXTo(a) => todo!(),
                Operation::EXTreg(a) => todo!(),
                Operation::NegB(a) => todo!(),
                Operation::Neg(a) => todo!(),
                Operation::Pop(a) => todo!(),
                Operation::Push(a) => todo!(),
                Operation::Trap(a) => todo!(),
                Operation::Ashr(a, b) => todo!(),
                Operation::Cmpd1(a, b) => todo!(),
                Operation::Cmpd2(a, b) => todo!(),
                Operation::Cmpi1(a, b) => todo!(),
                Operation::Cmpi2(a, b) => todo!(),
                Operation::Rol(a, b) => todo!(),
                Operation::Ror(a, b) => todo!(),
                Operation::Shl(a, b) => todo!(),
                Operation::Shr(a, b) => todo!(),
                Operation::Mul(a, b) => todo!(),
                Operation::Mulu(a, b) => todo!(),
                Operation::PRIOR(a, b) => todo!(),
                Operation::SCXT(a, b) => todo!(),
            }

            self.instruction_pointer += instr.len() as u16;
        }
    }

    pub fn read_byte(&self, address: usize) -> u8 {
        self.memory[address]
    }

    pub fn write_word(&mut self, address: usize, word: u16) {
        let [b1, b2] = word.to_le_bytes();
        self.memory[address] = b1;
        self.memory[address + 1] = b2;
    }

    pub fn write_byte(&mut self, address: usize, byte: u8) {
        self.memory[address] = byte;
    }

    pub fn read_word(&self, address: usize) -> u16 {
        u16::from_le_bytes([self.memory[address], self.memory[address + 1]])
    }

    pub fn read_gpr_word(&self, gpr: u8) -> u16 {
        self.read_word(self.gpr_address(gpr))
    }

    pub fn gpr_address(&self, gpr: u8) -> usize {
        registers::CP::ADDRESS + ((gpr as usize & 0xF) * 2)
    }

    pub fn read_gpr_byte(&self, gpr: u8) -> u8 {
        self.memory[self.read_sfr_word::<registers::CP>() as usize + (gpr as usize & 0xF)]
    }

    pub fn get_ip(&self) -> usize {
        self.instruction_pointer as usize
            | ((self.read_sfr_word::<registers::CSP>() as usize) << 16)
    }

    pub fn read_sfr_word<S: SpecialFunctionRegister>(&self) -> u16 {
        self.read_word(S::ADDRESS)
    }

    pub fn read_sfr_byte<S: SpecialFunctionRegister>(&self) -> u8 {
        self.memory[S::ADDRESS]
    }

    pub fn read_dpp_addressed_word(&self, address: u16) -> u16 {
        self.read_word(self.get_dpp_address(address))
    }
    pub fn read_dpp_addressed_byte(&self, address: u16) -> u8 {
        self.read_byte(self.get_dpp_address(address))
    }

    pub fn get_dpp_address(&self, address: u16) -> usize {
        let dpp = match address >> 14 {
            0 => self.read_sfr_word::<registers::DPP0>(),
            1 => self.read_sfr_word::<registers::DPP1>(),
            2 => self.read_sfr_word::<registers::DPP2>(),
            3 => self.read_sfr_word::<registers::DPP3>(),
            _ => panic!(),
        } as usize;
        let addr = (address & 0x3FFF) as usize;

        addr | (dpp << 14)
    }

    pub fn write_dpp_addressed_word(&mut self, address: u16, word: u16) {
        self.write_word(self.get_dpp_address(address), word);
    }
    pub fn write_dpp_addressed_byte(&mut self, address: u16, byte: u8) {
        self.write_byte(self.get_dpp_address(address), byte);
    }

    pub fn write_gpr_word(&mut self, gpr: u8, word: u16) {
        let addr = self.gpr_address(gpr);
        self.write_word(addr, word);
    }
    pub fn write_gpr_byte(&mut self, gpr: u8, byte: u8) {
        let addr = self.gpr_address(gpr);
        self.write_byte(addr, byte);
    }

    pub fn write_bit(&mut self, address: Address, bit: bool) -> Result {
        if let Address::Bitaddr(addr, bitn) = address {
            let byte = self.memory.get_mut(addr as usize).unwrap();
            *byte = *byte & (if bit { 1 } else { 0 } << bitn);

            Ok(())
        } else {
            Err(Error::BadArgs)
        }
    }

    pub fn read_bit(&mut self, address: Address) -> Result<bool> {
        if let Address::Bitaddr(addr, bitn) = address {
            let byte = self.memory[addr as usize];

            Ok(byte >> bitn == 1)
        } else {
            Err(Error::BadArgs)
        }
    }

    mk_write_fn!(byte, u8);
    mk_write_fn!(word, u16);
    mk_read_fn!(byte, u8);
    mk_read_fn!(word, u16);
}
