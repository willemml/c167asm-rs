use std::num::Wrapping;

use crate::Error;
use crate::Result;
use crate::addressing::Address;
use crate::addressing::ConditionCode;
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
                        let base = if self.extr {
                            0xEF00
                        } else {
                            0xFE00
                        };
                        self.${concat(write_, $type)}(a as usize * 2 + base, val)
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
                        let base = if self.extr {
                            0xEF00
                        } else {
                            0xFE00
                        };
                        self.${concat(read_, $type)}(a as usize * 2 + base)
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
                    self.${concat(read_dpp_addressed_, $type)}(self.read_gpr_word(a).wrapping_add(c))
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
    ext_count: usize,
    extr: bool,
    ext_addr: ExtAddr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtAddr {
    None,
    Page(u16),
    Seg(u8),
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

impl Flags {
    pub fn test_cc(&self, code: ConditionCode) -> bool {
        match code {
            ConditionCode::Unconditional => true,
            ConditionCode::ZoEQ => self.z,
            ConditionCode::NZoNE => !self.z,
            ConditionCode::V => self.v,
            ConditionCode::NV => !self.v,
            ConditionCode::N => self.n,
            ConditionCode::NN => !self.n,
            ConditionCode::CoULT => self.c,
            ConditionCode::NCoUGE => !self.c,
            ConditionCode::ULE => self.z || self.c,
            ConditionCode::UGT => !(self.z || self.c),
            ConditionCode::SLT => self.n ^ self.v,
            ConditionCode::SLE => self.z || (self.n ^ self.v),
            ConditionCode::SGE => !(self.n ^ self.v),
            ConditionCode::SGT => !(self.z || (self.n ^ self.v)),
            ConditionCode::NET => self.z || self.e,
        }
    }
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
    (word:$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $({$r:ident})? $self:ident, ($c1:ident, $val:ident) => $code:block) => {{
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
        $(${ignore($r)} a_initial)?
    }};
    (nc:$t:ident$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $({$r:ident})? $self:ident) => {
        arithmetic!($t:$(($nw))? $fn($a,$b) with $({$r})? $self, (c1, val) => {c1})
    };
    (c:$t:ident$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $({$r:ident})? $self:ident) => {
        arithmetic!($t:$(($nw))? $fn($a,$b) with $({$r})? $self, (c1, val) => {
            if $self.flags.c {
                let (valc, c2) = val.$fn(1);
                val = valc;
                c1 || c2
            } else { c1 }
        })
    };
    (byte:$(($nw:ident))? $fn:ident($a:ident, $b:ident) with $({$r:ident})? $self:ident, ($c1:ident, $val:ident) => $code:block) => {{
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
        $(${ignore($r)} a_initial)?
    }};
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            memory: vec![0u8; 16_000_000],
            instruction_pointer: 0,
            flags: Flags::default(),
            ext_count: 0,
            extr: false,
            ext_addr: ExtAddr::None,
        }
    }

    pub fn execute(&mut self) -> Result {
        let mut last = None;
        let mut repeat_count = 0;
        loop {
            let ip = self.get_ip();
            let instr = {
                let mut slice = &self.memory[ip..ip + 4];
                Instruction::read(&mut slice)?
            };
            let op = Operation::from(instr);

            if self.ext_count > 0 {
                self.ext_count -= 1;
            } else {
                self.extr = false;
                self.ext_addr = ExtAddr::None;
            }

            if Some(op) == last {
                if repeat_count == 0 {
                    println!("  repeating...");
                }
                repeat_count += 1;
            } else {
                if repeat_count > 1 {
                    println!("  repeated {} times", repeat_count);
                } else if repeat_count == 1 {
                    println!("  repeated once");
                }
                repeat_count = 0;
                println!("{:06x}: {}", self.get_ip(), op);
                last = Some(op);
            }

            // IP is always equal to the address of the instruction following a branch
            self.instruction_pointer += instr.len() as u16;

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
                Operation::Cmpi1(a, b) => {
                    let av = arithmetic!(nc:word overflowing_sub(a, b) with {ret} self);
                    self.write_address_word::<0>(a, av.wrapping_add(1))?;
                }
                Operation::Cmpi2(a, b) => {
                    let av = arithmetic!(nc:word overflowing_sub(a, b) with {ret} self);
                    self.write_address_word::<0>(a, av.wrapping_add(2))?;
                }
                Operation::Cmpd1(a, b) => {
                    let av = arithmetic!(nc:word overflowing_sub(a, b) with {ret} self);
                    self.write_address_word::<0>(a, av.wrapping_sub(1))?;
                }
                Operation::Cmpd2(a, b) => {
                    let av = arithmetic!(nc:word overflowing_sub(a, b) with {ret} self);
                    self.write_address_word::<0>(a, av.wrapping_sub(2))?;
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
                Operation::BFLDL(a, b, c) => {
                    if let (Address::Bitoff(addr), Address::Mask8(mask), Address::Data8(data)) =
                        (a, b, c)
                    {
                        let w = self.read_word(addr as usize + 0xFD00);
                        let nw = (w & ((!mask) as u16 | 0xFF00)) | (data as u16);
                        self.write_word(addr as usize + 0xFD00, nw);

                        self.flags.e = false;
                        self.flags.z = nw == 0;
                        self.flags.v = false;
                        self.flags.c = false;
                        self.flags.n = nw >> 15 == 1;
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::BFLDH(a, b, c) => {
                    if let (Address::Bitoff(addr), Address::Mask8(mask), Address::Data8(data)) =
                        (a, b, c)
                    {
                        let w = self.read_word(addr as usize + 0xFD00);
                        let nw = (w & (((!mask) as u16) << 8 | 0xFF)) | ((data as u16) << 8);
                        self.write_word(addr as usize + 0xFD00, nw);

                        self.flags.e = false;
                        self.flags.z = nw == 0;
                        self.flags.v = false;
                        self.flags.c = false;
                        self.flags.n = nw >> 15 == 1;
                    } else {
                        return Err(Error::BadArgs);
                    }
                }

                Operation::Pop(a) => {
                    let val = self.stack_pop();
                    self.write_address_word::<0>(a, val)?;
                }
                Operation::Push(a) => {
                    let val = self.read_address_word::<0>(a)?;
                    self.stack_push(val);
                }

                Operation::DISWDT() => todo!(),
                Operation::SRVWDT() => todo!(),

                Operation::EINIT() => {}
                Operation::IDLE() => {}
                Operation::NOP() => {}
                Operation::PWRDN() => return Ok(()),
                Operation::SRST() => todo!(),

                Operation::JB(a, b) => {
                    if let Address::Rel(r) = b {
                        if self.read_bit(a)? {
                            self.instruction_pointer = self
                                .instruction_pointer
                                .saturating_add_signed(r as i8 as i16 * 2);
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::JBC(a, b) => {
                    if let Address::Rel(r) = b {
                        if self.read_bit(a)? {
                            self.write_bit(a, false)?;
                            self.instruction_pointer = self
                                .instruction_pointer
                                .saturating_add_signed(r as i8 as i16 * 2);
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::JNB(a, b) => {
                    if let Address::Rel(r) = b {
                        if !self.read_bit(a)? {
                            self.instruction_pointer = self
                                .instruction_pointer
                                .saturating_add_signed(r as i8 as i16 * 2);
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::JNBS(a, b) => {
                    if let Address::Rel(r) = b {
                        if !self.read_bit(a)? {
                            self.write_bit(a, true)?;
                            // TODO: handle over/under flow
                            self.instruction_pointer = self
                                .instruction_pointer
                                .saturating_add_signed(r as i8 as i16 * 2);
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }

                Operation::JmpA(a, b) => {
                    if let (Address::CC(cc), Address::Caddr(addr)) = (a, b) {
                        if self.flags.test_cc(cc) {
                            self.instruction_pointer = addr;
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::JmpI(a, b) => {
                    if let Address::CC(cc) = a {
                        if self.flags.test_cc(cc) {
                            self.instruction_pointer = self.read_address_word::<0>(b)?;
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::JmpR(cc, r) => {
                    if self.flags.test_cc(cc) {
                        // TODO: handle over/under flow
                        self.instruction_pointer = self
                            .instruction_pointer
                            .saturating_add_signed(r as i8 as i16);
                    }
                }
                Operation::JmpS(a, b) => {
                    if let (Address::Seg(seg), Address::Caddr(addr)) = (a, b) {
                        self.instruction_pointer = addr;
                        // upper 8 bits of CSP are unused
                        self.write_sfr_word::<registers::CSP>(seg as u16);
                    } else {
                        return Err(Error::BadArgs);
                    }
                }

                Operation::RET() => {
                    self.instruction_pointer = self.stack_pop();
                }
                Operation::RETI() => {
                    self.instruction_pointer = self.stack_pop();
                    let csp = self.stack_pop();
                    self.write_sfr_word::<registers::CSP>(csp);
                    self.stack_pop();
                }
                Operation::RETS() => {
                    self.instruction_pointer = self.stack_pop();
                    let csp = self.stack_pop();
                    self.write_sfr_word::<registers::CSP>(csp);
                }
                Operation::RETP(a) => {
                    self.instruction_pointer = self.stack_pop();
                    let w = self.stack_pop();
                    self.flags.e = w as i16 == i16::MIN;
                    self.flags.z = w == 0;
                    self.flags.n = w >> 15 == 1;
                    self.write_address_word::<0>(a, w)?;
                }

                Operation::PCall(a, b) => {
                    if let Address::Caddr(addr) = b {
                        let w = self.read_address_word::<0>(a)?;
                        self.stack_push(w);
                        self.stack_push(self.instruction_pointer);
                        self.instruction_pointer = addr;
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::CallA(a, b) => {
                    if let (Address::CC(cc), Address::Caddr(addr)) = (a, b) {
                        if self.flags.test_cc(cc) {
                            self.stack_push(self.instruction_pointer);
                            self.instruction_pointer = addr;
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::CallI(a, b) => {
                    if let Address::CC(cc) = a {
                        if self.flags.test_cc(cc) {
                            self.stack_push(self.instruction_pointer);
                            self.instruction_pointer = self.read_address_word::<0>(b)?;
                        }
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::CallR(a) => {
                    if let Address::Rel(r) = a {
                        self.stack_push(self.instruction_pointer);
                        // TODO: handle over/under flow
                        self.instruction_pointer = self
                            .instruction_pointer
                            .saturating_add_signed(r as i8 as i16);
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::CallS(a, b) => {
                    if let (Address::Seg(seg), Address::Caddr(addr)) = (a, b) {
                        self.stack_push(self.read_sfr_word::<registers::CSP>());
                        self.stack_push(self.instruction_pointer);
                        self.instruction_pointer = addr;
                        // upper 8 bits of CSP are unused
                        self.write_sfr_word::<registers::CSP>(seg as u16);
                    } else {
                        return Err(Error::BadArgs);
                    }
                }

                Operation::Rol(a, b) => {
                    let w = self.read_address_word::<0>(a)?;
                    let r = self.read_address_byte::<0>(b)?;
                    self.write_address_word::<0>(a, w.rotate_left(r as u32))?;
                }
                Operation::Ror(a, b) => {
                    let w = self.read_address_word::<0>(a)?;
                    let r = self.read_address_byte::<0>(b)?;
                    self.write_address_word::<0>(a, w.rotate_right(r as u32))?;
                }

                Operation::EXTo(a) => {
                    if let Address::EXTSeq(i) = a {
                        match i {
                            crate::addressing::EXTSeq::EXTP(pag10, irang2) => {
                                self.ext_count = irang2.0 as usize;
                                self.ext_addr = ExtAddr::Page(pag10.0);
                            }
                            crate::addressing::EXTSeq::EXTPR(pag10, irang2) => {
                                self.ext_count = irang2.0 as usize;
                                self.extr = true;
                                self.ext_addr = ExtAddr::Page(pag10.0);
                            }
                            crate::addressing::EXTSeq::EXTS(seg, irang2) => {
                                self.ext_count = irang2.0 as usize;
                                self.ext_addr = ExtAddr::Seg(seg.0);
                            }
                            crate::addressing::EXTSeq::EXTSR(seg, irang2) => {
                                self.ext_count = irang2.0 as usize;
                                self.extr = true;
                                self.ext_addr = ExtAddr::Seg(seg.0);
                            }
                        }
                        self.ext_count += 1;
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::EXTreg(a) => {
                    if let Address::EXTRSeq(i) = a {
                        match i {
                            crate::addressing::EXTRSeq::EXTP(gpr, irang2) => {
                                let page = self.read_gpr_word(gpr.0);
                                self.ext_count = irang2.0 as usize;
                                self.ext_addr = ExtAddr::Page(page);
                            }
                            crate::addressing::EXTRSeq::EXTPR(gpr, irang2) => {
                                let page = self.read_gpr_word(gpr.0);
                                self.ext_count = irang2.0 as usize;
                                self.ext_addr = ExtAddr::Page(page);
                                self.extr = true;
                            }
                            crate::addressing::EXTRSeq::EXTS(gpr, irang2) => {
                                let seg = (self.read_gpr_word(gpr.0) >> 8) as u8;
                                self.ext_count = irang2.0 as usize;
                                self.ext_addr = ExtAddr::Seg(seg);
                            }
                            crate::addressing::EXTRSeq::EXTSR(gpr, irang2) => {
                                let seg = (self.read_gpr_word(gpr.0) >> 8) as u8;
                                self.ext_count = irang2.0 as usize;
                                self.ext_addr = ExtAddr::Seg(seg);
                                self.extr = true;
                            }
                        }
                        self.ext_count += 1;
                    } else {
                        return Err(Error::BadArgs);
                    }
                }
                Operation::AtEx(a) => todo!(),

                Operation::CPLB(a) => todo!(),
                Operation::CPL(a) => todo!(),
                Operation::Div(a) => todo!(),
                Operation::Divl(a) => todo!(),
                Operation::Divlu(a) => todo!(),
                Operation::Divu(a) => todo!(),
                Operation::NegB(a) => todo!(),
                Operation::Neg(a) => todo!(),
                Operation::Trap(a) => todo!(),
                Operation::Ashr(a, b) => {
                    let w = self.read_address_word::<0>(a)?;
                    let s = self.read_address_word::<0>(b)? & 0b1111;

                    let v = w << s;
                }
                Operation::Shl(a, b) => {
                    let w = self.read_address_word::<0>(a)?;
                    let s = self.read_address_word::<0>(b)? & 0b1111;

                    let v = w << s;

                    self.flags.e = false;
                    self.flags.z = 0 == v;
                    self.flags.v = false;
                    self.flags.c = (1 << (16 - s)) & w != 0;
                    self.flags.n = v >> 15 == 1;

                    self.write_address_word::<0>(a, v)?;
                }
                Operation::Shr(a, b) => {
                    let w = self.read_address_word::<0>(a)?;
                    let s = self.read_address_word::<0>(b)? & 0b1111;

                    let v = w >> s;

                    self.flags.e = false;
                    self.flags.z = 0 == v;
                    self.flags.v = w << (16 - s) == 0;
                    self.flags.c = (1 << s) & w != 0;
                    self.flags.n = v >> 15 == 1;

                    self.write_address_word::<0>(a, v)?;
                }
                Operation::Mul(a, b) => todo!(),
                Operation::Mulu(a, b) => todo!(),
                Operation::PRIOR(a, b) => todo!(),
                Operation::SCXT(a, b) => todo!(),
            }
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
        self.read_word(self.gpr_address_word(gpr))
    }

    pub fn gpr_address_word(&self, gpr: u8) -> usize {
        self.read_sfr_word::<registers::CP>() as usize + ((gpr as usize & 0xF) * 2)
    }
    pub fn gpr_address_byte(&self, gpr: u8) -> usize {
        self.read_sfr_word::<registers::CP>() as usize + (gpr as usize & 0xF)
    }

    pub fn stack_push(&mut self, word: u16) {
        let mut sp = self.read_sfr_word::<registers::SP>();
        let stkov = self.read_sfr_word::<registers::STKOV>();

        sp -= 2;

        // TODO: make sure stack pointer can only be set to a multiple of two
        // TODO: handle stack overflow/underflow trap
        if sp < stkov {
            println!("STKOV: {:04X}", sp);
            todo!();
        }

        self.write_sfr_word::<registers::SP>(sp);

        self.write_word(sp as usize, word);
    }

    pub fn stack_pop(&mut self) -> u16 {
        let mut sp = self.read_sfr_word::<registers::SP>();
        let stkun = self.read_sfr_word::<registers::STKUN>();

        sp += 2;

        if sp > stkun {
            println!("STKUN: {:04X}", sp);
            todo!();
        }

        self.write_sfr_word::<registers::SP>(sp);

        self.read_word(sp as usize)
    }

    pub fn read_gpr_byte(&self, gpr: u8) -> u8 {
        self.memory[self.gpr_address_byte(gpr)]
    }

    pub fn get_ip(&self) -> usize {
        self.instruction_pointer as usize
            | ((self.read_sfr_word::<registers::CSP>() as usize) << 16)
    }

    pub fn read_sfr_word<S: SpecialFunctionRegister>(&self) -> u16 {
        self.read_word(S::ADDRESS)
    }
    pub fn write_sfr_word<S: SpecialFunctionRegister>(&mut self, word: u16) {
        self.write_word(S::ADDRESS, word)
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
        match self.ext_addr {
            ExtAddr::Page(p) => ((p as usize & 0x3FF) << 14) & (address as usize & 0x3FFF),
            ExtAddr::Seg(s) => ((s as usize) << 16) & address as usize,
            ExtAddr::None => {
                let dpp = match address >> 14 {
                    0 => self.read_sfr_word::<registers::DPP0>(),
                    1 => self.read_sfr_word::<registers::DPP1>(),
                    2 => self.read_sfr_word::<registers::DPP2>(),
                    3 => self.read_sfr_word::<registers::DPP3>(),
                    _ => panic!(),
                } as usize
                    & 0x3FF; // keep only lower 10 bits
                let addr = (address & 0x3FFF) as usize;

                addr | (dpp << 14)
            }
        }
    }

    pub fn write_dpp_addressed_word(&mut self, address: u16, word: u16) {
        self.write_word(self.get_dpp_address(address), word);
    }
    pub fn write_dpp_addressed_byte(&mut self, address: u16, byte: u8) {
        self.write_byte(self.get_dpp_address(address), byte);
    }

    pub fn write_gpr_word(&mut self, gpr: u8, word: u16) {
        let addr = self.gpr_address_word(gpr);
        self.write_word(addr, word);
    }
    pub fn write_gpr_byte(&mut self, gpr: u8, byte: u8) {
        let addr = self.gpr_address_byte(gpr);
        self.write_byte(addr, byte);
    }

    pub fn write_bit(&mut self, address: Address, bit: bool) -> Result {
        if let Address::Bitaddr(addr, bitn) = address {
            let mut word = self.read_word(addr as usize + 0xFD00);
            word = word & (if bit { 1 } else { 0 } << bitn);

            self.write_word(addr as usize + 0xFD00, word);

            Ok(())
        } else {
            Err(Error::BadArgs)
        }
    }

    pub fn read_bit(&mut self, address: Address) -> Result<bool> {
        if let Address::Bitaddr(addr, bitn) = address {
            let word = self.read_word(addr as usize + 0xFD00);

            Ok(word >> bitn == 1)
        } else {
            Err(Error::BadArgs)
        }
    }

    mk_write_fn!(byte, u8);
    mk_write_fn!(word, u16);
    mk_read_fn!(byte, u8);
    mk_read_fn!(word, u16);
}
