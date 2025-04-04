use super::*;

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

impl Interpreter {
    mk_write_fn!(byte, u8);
    mk_write_fn!(word, u16);
    mk_read_fn!(byte, u8);
    mk_read_fn!(word, u16);
}
