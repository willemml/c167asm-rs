use std::collections::HashMap;
use std::num::Wrapping;
use std::process::exit;

mod memory;
mod ops;

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

pub enum OpResult {
    Jump(usize),
    PowerDown,
    Reset,
    Continue,
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

#[derive(Clone, Debug)]
pub struct Block {
    pub start: usize,
    pub end: usize,
    pub branches: Vec<(usize, Instruction)>,
}

#[derive(Clone, Debug)]
pub struct BlockJumps {
    pub start: usize,
    pub end: usize,
    pub branches: HashMap<usize, usize>,
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

    pub fn flow(&mut self) -> Result {
        let mut starts = Vec::new();
        let mut processed = Vec::new();
        let mut todo = vec![self.next_block()?];

        while !todo.is_empty() {
            let current = todo.remove(todo.len() - 1);

            starts.push(current.start);

            let mut branches = HashMap::new();

            for branch in current.branches.iter() {
                self.do_op((*branch).1.into(), true)?;

                let ip = self.get_ip();

                branches.insert(branch.0, ip);

                if starts.contains(&ip) {
                    continue;
                } else {
                    starts.push(ip);
                }

                if let Ok(block) = self.next_block() {
                    todo.push(block);
                }
            }

            processed.push(BlockJumps {
                branches,
                start: current.start,
                end: current.end,
            });
        }
        self.write_sfr_word::<registers::DPP1>(0x204);
        self.write_sfr_word::<registers::DPP2>(0x204);
        self.write_sfr_word::<registers::DPP3>(0x204);
        self.write_sfr_word::<registers::DPP0>(0x204);

        for block in processed {
            let mut ip = block.start;
            if ip <= 0x800000 {
                continue;
            }
            if ip >= 0x820000 && ip <= 0x820202 {
                continue;
            }
            println!();
            println!("==== {:06x} ====", ip);
            while ip <= block.end {
                let instr = {
                    let mut slice = &self.memory[ip..ip + 4];
                    if slice == [0, 0, 0, 0] {
                        println!("--- zeros");
                        break;
                    }
                    Instruction::read(&mut slice)?
                };
                let op = Operation::from(instr);
                print!("{:06X}: {}", ip, op);
                if op.jump_info().is_none() {
                    self.instruction_pointer = ip as u16;
                    self.write_sfr_word::<registers::CSP>((ip >> 16) as u16);
                    match op {
                        Operation::EXTo(_) | Operation::EXTreg(_) => {
                            self.do_op(op, true)?;
                        }
                        _ => {}
                    }
                }
                if let Some(to) = block.branches.get(&ip) {
                    println!(" ----> 0x{:06x}", to);
                } else {
                    println!();
                    for a in op.args() {
                        if let Address::Mem(m) = a {
                            let addr = self.get_dpp_address(m);
                            println!("  - {:06X}", addr);
                            if [0x13E7A, 0x10FC8, 0x11B21, 0x1398C, 0x13974, 0x11B9A]
                                .contains(&(addr & 0xFFFFF))
                            {
                                println!("!!!!!!!!!!!!!!!!!!!!FOUND!!!!!!!!!!!!!!!!!!!!")
                            }
                        }
                    }
                }
                ip += instr.len();
                if self.ext_count > 0 {
                    self.ext_count -= 1;
                } else {
                    self.extr = false;
                    self.ext_addr = ExtAddr::None;
                }
            }
        }

        Ok(())
    }

    pub fn next_block(&mut self) -> Result<Block> {
        let start = self.get_ip();
        let mut end;
        let mut branches = Vec::new();
        loop {
            let ip = self.get_ip();
            let instr = {
                let mut slice = &self.memory[ip..ip + 4];
                Instruction::read(&mut slice)?
            };
            end = ip;
            let op = Operation::from(instr);

            match op {
                Operation::SRST() | Operation::PWRDN() | Operation::IDLE() => break,
                _ => {}
            }

            if let Some(ji) = op.jump_info() {
                if !ji.conditional && ji.to.is_some_and(|a| a.is_constant()) && !ji.rel {
                    branches.push((ip, instr));
                }
                if ji.is_ret {
                    break;
                }
            }

            if self.instruction_pointer >= u16::MAX - instr.len() as u16 {
                break;
            }
            self.instruction_pointer += instr.len() as u16;
        }

        Ok(Block {
            start,
            end,
            branches,
        })
    }

    pub fn execute(&mut self) -> Result {
        // execute code, check if instruction is a jump
        // if the jump is unconditional keep going,
        // if it is conditional add it to the jump list and keep executing (respecting conditional) until repeat detected
        // when repeat is detected run jump with opposite condition
        // only count repeat jump
        // ignore returns
        // keep going until repeat detected with no jumps in list
        // keep track of all addresses where a jump occurs, unconditional jump means the end of a code section, jump arrival is the start of a code section

        let mut jumps: Vec<(bool, usize, JumpInfo)> = Vec::new();

        loop {
            let ip = self.get_ip();
            let instr = {
                let mut slice = &self.memory[ip..ip + 4];
                Instruction::read(&mut slice)?
            };
            let op = Operation::from(instr);
            println!("{:06X}: {}", ip, op);

            let mut repeat = None;
            let jump = if let Some(ji) = op.jump_info() {
                if ji.conditional {
                    let jmp = {
                        let iter = jumps.clone().into_iter();
                        let jmp = iter.enumerate().find(|(_, (_, _, j))| j == &ji);
                        jmp
                    };
                    if let Some((i, (c, o, _))) = jmp {
                        repeat = Some((c, o));
                        jumps.remove(i);
                    }
                    Some(ji)
                } else {
                    None
                }
            } else {
                None
            };

            // IP is always equal to the address of the instruction following a branch
            self.instruction_pointer += instr.len() as u16;
            match self.do_op(op, false)? {
                OpResult::Jump(_) => {
                    if let Some(ji) = jump {
                        if let Some((c, o)) = repeat {
                            if c {
                                self.do_op(ji.op, true)?;
                                println!("forced");
                            } else {
                                // TODO: add instruction length
                                self.instruction_pointer = (o & 0xFFFF) as u16;
                                self.write_sfr_word::<registers::CSP>((o >> 16) as u16);
                                println!("ignored");
                            }
                        } else {
                            jumps.push((false, ip, ji));
                            println!("found");
                        }
                    }
                }
                OpResult::PowerDown | OpResult::Reset => {
                    self.instruction_pointer = 0;
                    self.write_sfr_word::<registers::CSP>(0);
                }
                OpResult::Continue => {
                    if let Some(jump) = jump {
                        jumps.push((true, ip, jump))
                    }
                }
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
        // let sp = self.read_sfr_word::<registers::SP>() - 2;
        // let stkov = self.read_sfr_word::<registers::STKOV>();

        // // TODO: make sure stack pointer can only be set to a multiple of two
        // // TODO: handle stack overflow/underflow trap
        // if sp < stkov {
        //     println!("STKOV: {:04X}", sp);
        // } else {
        //     self.write_sfr_word::<registers::SP>(sp);

        //     self.write_word(sp as usize, word);
        // }
    }

    pub fn stack_pop(&mut self) -> u16 {
        // let sp = self.read_sfr_word::<registers::SP>();
        // let stkun = self.read_sfr_word::<registers::STKUN>();

        // if sp > stkun {
        //     println!("STKUN: {:04X}", sp);
        //     0
        // } else {
        //     self.write_sfr_word::<registers::SP>(sp + 2);
        //     self.read_word(sp as usize)
        // }
        0
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
}
