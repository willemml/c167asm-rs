use super::*;

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
    pub fn do_op(&mut self, op: Operation, ignore_cc: bool) -> Result<OpResult, Error> {
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
            Operation::SRVWDT() => {
                // TODO: implement watchdog
            }

            Operation::EINIT() => {}
            Operation::IDLE() => {}
            Operation::NOP() => {}
            Operation::PWRDN() => return Ok(OpResult::PowerDown),
            Operation::SRST() => todo!(),

            Operation::JB(a, b) => {
                if let Address::Rel(r) = b {
                    if self.read_bit(a)? || ignore_cc {
                        self.instruction_pointer = self
                            .instruction_pointer
                            .saturating_add_signed(r as i8 as i16 * 2);
                        return Ok(OpResult::Jump(self.get_ip()));
                    }
                } else {
                    return Err(Error::BadArgs);
                }
            }
            Operation::JBC(a, b) => {
                if let Address::Rel(r) = b {
                    if self.read_bit(a)? || ignore_cc {
                        self.write_bit(a, false)?;
                        self.instruction_pointer = self
                            .instruction_pointer
                            .saturating_add_signed(r as i8 as i16 * 2);
                        return Ok(OpResult::Jump(self.get_ip()));
                    }
                } else {
                    return Err(Error::BadArgs);
                }
            }
            Operation::JNB(a, b) => {
                if let Address::Rel(r) = b {
                    if !self.read_bit(a)? || ignore_cc {
                        self.instruction_pointer = self
                            .instruction_pointer
                            .saturating_add_signed(r as i8 as i16 * 2);
                        return Ok(OpResult::Jump(self.get_ip()));
                    }
                } else {
                    return Err(Error::BadArgs);
                }
            }
            Operation::JNBS(a, b) => {
                if let Address::Rel(r) = b {
                    if !self.read_bit(a)? || ignore_cc {
                        self.write_bit(a, true)?;
                        // TODO: handle over/under flow
                        self.instruction_pointer = self
                            .instruction_pointer
                            .saturating_add_signed(r as i8 as i16 * 2);
                        return Ok(OpResult::Jump(self.get_ip()));
                    }
                } else {
                    return Err(Error::BadArgs);
                }
            }

            Operation::JmpA(a, b) => {
                if let (Address::CC(cc), Address::Caddr(addr)) = (a, b) {
                    if self.flags.test_cc(cc) || ignore_cc {
                        self.instruction_pointer = addr;
                        return Ok(OpResult::Jump(self.get_ip()));
                    }
                } else {
                    return Err(Error::BadArgs);
                }
            }
            Operation::JmpI(a, b) => {
                if let Address::CC(cc) = a {
                    if self.flags.test_cc(cc) || ignore_cc {
                        self.instruction_pointer = self.read_address_word::<0>(b)?;
                        return Ok(OpResult::Jump(self.get_ip()));
                    }
                } else {
                    return Err(Error::BadArgs);
                }
            }
            Operation::JmpR(cc, r) => {
                if self.flags.test_cc(cc) || ignore_cc {
                    // TODO: handle over/under flow
                    self.instruction_pointer = self
                        .instruction_pointer
                        .saturating_add_signed(r as i8 as i16);
                    return Ok(OpResult::Jump(self.get_ip()));
                }
            }
            Operation::JmpS(a, b) => {
                if let (Address::Seg(seg), Address::Caddr(addr)) = (a, b) {
                    self.instruction_pointer = addr;
                    // upper 8 bits of CSP are unused
                    self.write_sfr_word::<registers::CSP>(seg as u16);
                    return Ok(OpResult::Jump(self.get_ip()));
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
                    if self.flags.test_cc(cc) || ignore_cc {
                        self.stack_push(self.instruction_pointer);
                        self.instruction_pointer = addr;
                    }
                } else {
                    return Err(Error::BadArgs);
                }
            }
            Operation::CallI(a, b) => {
                if let Address::CC(cc) = a {
                    if self.flags.test_cc(cc) || ignore_cc {
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

            Operation::CPL(a) => {
                let av = self.read_address_word::<0>(a)?;
                self.write_address_word::<0>(a, !av)?;
                self.flags.e = av as i16 == i16::MIN;
                self.flags.z = !av == 0;
                self.flags.v = false;
                self.flags.c = false;
                self.flags.n = !av >> 15 == 1;
            }
            Operation::CPLB(a) => {
                let av = self.read_address_byte::<0>(a)?;
                self.write_address_byte::<0>(a, !av)?;
                self.flags.e = av as i8 == i8::MIN;
                self.flags.z = !av == 0;
                self.flags.v = false;
                self.flags.c = false;
                self.flags.n = !av >> 7 == 1;
            }
            Operation::Div(a) => todo!(),
            Operation::Divl(a) => todo!(),
            Operation::Divlu(a) => todo!(),
            Operation::Divu(a) => todo!(),
            Operation::NegB(a) => todo!(),
            Operation::Neg(a) => todo!(),
            Operation::Trap(a) => todo!(),
            Operation::Ashr(a, b) => {
                let w = self.read_address_word::<0>(a)? as i16;
                let s = self.read_address_word::<0>(b)? & 0b1111;

                let v = w >> s;

                // TODO: Fix flag logic, use carry
                self.flags.e = false;
                self.flags.z = 0 == v;
                self.flags.v = s != 0 && w != 0;
                self.flags.c = (1 << (16 - s)) & w != 0;
                self.flags.n = v >> 15 == 1;

                self.write_address_word::<0>(a, v as u16)?;
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

        Ok(OpResult::Continue)
    }
}
