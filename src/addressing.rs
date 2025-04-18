use std::fmt::Display;

use crate::Error;
use crate::Result;

macro_rules! read {
    ($reader: ident, $n:expr) => {{
        let mut b = [0u8; $n];
        $reader.read_exact(&mut b).map_err(|e| {
            if e.kind() == std::io::ErrorKind::UnexpectedEof {
                Error::MissingArgs
            } else {
                e.into()
            }
        })?;
        b
    }};
}

macro_rules! read_fn {
    ($(:$p:vis)? [$($n:ident),*] -> $r:ty $code:block) => {
        $($p)? fn read<R: std::io::Read>(reader: &mut R) -> Result<$r> {
            let [$($n),*] = read!(reader, ${count($n)});

            Ok($code)
        }
    };
}
macro_rules! write_fn {
    ($(:$p:vis)? $sel:ident => $code:block) => {
        $($p)? fn write<W: std::io::Write>(&$sel, writer: &mut W) -> Result {
            writer.write_all(&$code)?;
            Ok(())
        }
    };
}

macro_rules! mk_address {
    {
        $($name:ident($type:ty)),*
    } => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub enum Address {$(
           $name($type),
        )*
            Bitaddr(u8, u8),
            Indirect16(u8, u16),
            EXTSeq(EXTSeq),
            EXTRSeq(EXTRSeq),
            CC(ConditionCode),
            AtEx(AtEx),
        }

        $(
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $name(pub $type);

        impl From<$name> for Address {
            fn from(value: $name) -> Self {
                Self::$name(value.0)
            }
        }

        impl TryFrom<Address> for $name {
            type Error = ();

            fn try_from(value: Address) -> Result<Self, Self::Error> {
                if let Address::$name(v) = value {
                    Ok(Self(v))
                } else {
                    Err(())
                }
            }
        }
        )*
    };
}

macro_rules! read_write {
    ($sel:ident $a:ident($ae:expr), $reader:ident => $code:block) => {
        impl $a {
            pub fn read<R: std::io::Read>($reader: &mut R) -> Result<C<$a>> $code
            write_fn!(:pub $sel => { [$ae] });
        }
    };
    ($sel:ident <$a:ident($ae:expr), $b:ident($be:expr)>, $reader:ident => $code:block) => {
        impl Arg for ($a, $b) {
            type S = ($a, $b);
            fn read<R: std::io::Read>($reader: &mut R) -> Result<Self> $code
            write_fn!($sel => { [$ae, $be].concat() });
        }
    };

    (4_4: $a:ident, $b:ident) => {
        read_write!(self <$a(&[self.0.0 << 4 | self.1.0]), $b([].as_slice())>, reader => {
            let [byte] = read!(reader, 1);

            Ok(($a(byte >> 4), $b(byte & 0x0F)))
        });
    };

    (4_16: $a:ident, $b:ident) => {
        read_write!(self <$a(&[self.0.0 | 0xF0]), $b(self.1.0.to_le_bytes().as_slice())>, reader => {
            let b = read!(reader, 3);

            Ok(($a(b[0] & 0x0F), $b(u16::from_le_bytes([b[1],b[2]]))))
        });
    };

    (8_16: $a:ident, $b:ident) => {
        read_write!(self <$a(&[self.0.0]), $b(self.1.0.to_le_bytes().as_slice())>, reader => {
            let b = read!(reader, 3);

            Ok(($a(b[0]), $b(u16::from_le_bytes([b[1], b[2]]))))
        });
    };
    (!8_16: $b:ident, $a:ident) => {
        impl Arg for ($a, $b) {
            type S = ($a, $b);
            fn read<R: std::io::Read>(reader: &mut R) -> Result<Self> {
                let [b1,b2,b3] = read!(reader, 3);
                Ok(($a(u16::from_le_bytes([b2, b3])), $b(b1)))
            }
            write_fn!(self => {
                let b = self.0.0.to_le_bytes();
                [self.1.0, b[0], b[1]]
            });
        }
    };

    (8_8: $a:ident, $b:ident) => {
        read_write!(self <$a(&[self.0.0]), $b([self.1.0, 0].as_slice())>, reader => {
            let [b1,b2,_] = read!(reader, 3);

            Ok(($a(b1), $b(b2)))
        });
    };

    (8(4)x2: $a:ident, $b:ident) => {
        read_write!(self <$a(&[self.0.0, self.1.0, (self.0.1 << 4) | self.1.1]), $b([].as_slice())>, reader => {
            let [b1,b2,b3] = read!(reader, 3);

            Ok(($a(b1, b3 >> 4), $b(b2, b3 & 0x0F)))
        });
    };

    (40: $a:ident) => {
        read_write!(self $a(self.0 << 4), reader => {
            let [byte] = read!(reader, 1);

            C($a(byte >> 4))
        });
    };
    (8: $a:ident) => {
        read_write!(self $a(self.0), reader => {
            let [byte] = read!(reader, 1);

            Ok(C($a(byte)))
        });
    };
}

macro_rules! read_44 {
    ($($t:ident),*) => {
    $(
        read_write!(4_4: GPR, $t);
        read_write!(4_4: $t, GPR);
        read_write!(4_4: Indirect, $t);
    )*
    };
}

mk_address! {
    GPR(u8),
    Indirect(u8),
    IndirectIncr(u8),
    IndirectDecr(u8),
    Data16(u16),
    Data8(u8),
    Data4(u8),
    Data3(u8),
    Mem(u16),
    Reg(u8),
    Special(u8),
    Mask8(u8),
    Bitoff(u8),
    Pag10(u16),
    Irang2(u8),
    Rel(u8),
    Caddr(u16),
    Trap7(u8),
    Seg(u8)
}

read_44!(Data4, Special, Indirect, IndirectDecr, IndirectIncr);

read_write!(4_4: GPR, GPR);
read_write!(4_16: GPR, Mem);
read_write!(4_16: GPR, Data16);
read_write!(8_8: Reg, Data8);
read_write!(8_16: Reg, Mem);
read_write!(8_16: Seg, Caddr);
read_write!(8_16: Reg, Data16);
read_write!(!8_16: Reg, Mem);
read_write!(8_16: Reg, Caddr);
read_write!(8_16: Indirect, Mem);
read_write!(!8_16: Indirect, Mem);
read_write!(8(4)x2: Bitaddr, Bitaddr);
read_write!(8: GPR);
read_write!(8: Bitoff);
read_write!(8: Rel);
read_write!(8: Reg);
read_write!(8: Trap7);

impl Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Address::GPR(n) => write!(f, "R{}", n & 0xF),
            Address::Indirect(n) => write!(f, "[R{}]", n & 0xF),
            Address::IndirectIncr(n) => write!(f, "[R{}+]", n & 0xF),
            Address::IndirectDecr(n) => write!(f, "[R{}-]", n & 0xF),
            Address::Data16(n) => write!(f, "#{}", n),
            Address::Data8(n) => write!(f, "#{}", n),
            Address::Data4(n) | Address::Data3(n) => write!(f, "#{}", n),
            Address::Mem(n) => write!(f, "*{:04X}h", n),
            Address::Reg(n) => {
                if 0xF0 & n == 0xF0 {
                    write!(f, "R{}", n & 0xF)
                } else {
                    // TODO: handle ESFR
                    write!(
                        f,
                        "{}",
                        crate::registers::sfr_name_from_phy(*n as u16 * 2 + 0xFE00).unwrap_or(
                            crate::registers::esfr_name_from_phy(*n as u16 * 2 + 0xEF00)
                                .unwrap_or(&format!("0x{:02X}", n))
                        )
                    )
                }
            }
            Address::Special(s) => {
                let mode = s >> 3;

                if mode == 0 {
                    write!(f, "#{}", s)
                } else {
                    let gpr = s & 0b11;
                    if (s >> 2) & 0b01 == 1 {
                        write!(f, "[R{}+]", gpr)
                    } else {
                        write!(f, "[R{}]", gpr)
                    }
                }
            }
            Address::Mask8(m) => write!(f, "#0b{:08b}", m),
            Address::Bitoff(a) => write!(f, "0x{:02X}", a),
            Address::Irang2(r) => write!(f, "{}", r),
            Address::Rel(r) => write!(f, "{}", *r as i8),
            Address::Caddr(c) => write!(f, "0x{:04X}h", c),
            Address::Seg(s) => write!(f, "0x{:02X}", s),
            Address::Bitaddr(a, b) => write!(f, "*{:02X}h({})", a, b),
            Address::Indirect16(n, c) => write!(f, "[R{}+#{:04X}h]", n, c),
            Address::CC(condition_code) => write!(f, "{:?}", condition_code),
            a => write!(f, "{:0X?}", a),
        }
    }
}

pub trait Arg {
    type S;
    fn read<R: std::io::Read>(reader: &mut R) -> Result<Self::S>;
    fn write<W: std::io::Write>(&self, writer: &mut W) -> Result;
}

impl Arg for () {
    type S = ();
    fn read<R: std::io::Read>(_reader: &mut R) -> Result {
        Ok(())
    }

    fn write<W: std::io::Write>(&self, _writer: &mut W) -> Result {
        Ok(())
    }
}

impl Irang2 {
    read_fn!(:pub [byte] -> C<Self> {
        C(Irang2((byte & 0b00110000) >> 4))
    });
    write_fn!(:pub self => {
        [self.0 << 4]
    });
}

impl Arg for (Indirect16, GPR) {
    type S = Self;
    read_fn!([b1,b2,b3] -> Self {(
        Indirect16(b1 & 0x0F, u16::from_le_bytes([b2, b3])),
        GPR(b1 >> 4),
    )});
    write_fn!(self => {
        let b = self.0.1.to_le_bytes();
        [(self.1.0 << 4) | self.0.0, b[0], b[1]]
    });
}

impl Arg for (GPR, Indirect16) {
    type S = Self;
    read_fn!([b1,b2,b3] -> Self {(
        GPR(b1 >> 4),
        Indirect16(b1 & 0x0F, u16::from_le_bytes([b2, b3])),
    )});
    write_fn!(self => {
        let b = self.1.1.to_le_bytes();
        [(self.0.0 << 4) | self.1.0, b[0], b[1]]
    });
}

impl Arg for (Bitoff, Mask8, Data8) {
    type S = Self;
    read_fn!([b1,b2,b3] -> Self {
        (Bitoff(b1), Mask8(b2), Data8(b3))
    });
    write_fn!(self => {
        [self.0.0, self.1.0, self.2.0]
    });
}

impl Arg for (ConditionCode, IndirectIncr) {
    type S = Self;
    read_fn!([byte] -> Self {(
        ConditionCode::from_repr(byte >> 4).unwrap(),
        IndirectIncr(byte & 0x0F),
    )});
    write_fn!(self => {
        [self.0 as u8, self.1.0]
    });
}
impl Arg for (ConditionCode, Indirect) {
    type S = Self;
    read_fn!([byte] -> Self {(
        ConditionCode::from_repr(byte >> 4).unwrap(),
        Indirect(byte & 0x0F),
    )});
    write_fn!(self => {
        [self.0 as u8, self.1.0]
    });
}
impl Arg for (ConditionCode, Caddr) {
    type S = Self;
    read_fn!([b1, b2, b3] -> Self {(
        ConditionCode::from_repr(b1 >> 4).unwrap(),
        Caddr(u16::from_le_bytes([b2, b3])),
    )});
    write_fn!(self => {
        let b = self.1.0.to_le_bytes();
        [self.0 as u8, b[0], b[1]]
    });
}
impl Arg for (Bitaddr, Rel) {
    type S = Self;
    read_fn!([b1,b2,b3] -> Self {
        (Bitaddr(b1, b3 >> 4), Rel(b2))
    });
    write_fn!(self => {
        [self.0.0, self.1.0, self.0.1 << 4]
    });
}

pub struct C<T>(pub T);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Bitaddr(pub u8, pub u8);
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Indirect16(pub u8, pub u16);

#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AtEx {
    Atomic(Irang2),
    EXTR(Irang2),
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum EXTRSeq {
    EXTP(GPR, Irang2) = 0b01,
    EXTPR(GPR, Irang2) = 0b11,
    EXTS(GPR, Irang2) = 0b00,
    EXTSR(GPR, Irang2) = 0b10,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum EXTSeq {
    EXTP(Pag10, Irang2) = 0b01,
    EXTPR(Pag10, Irang2) = 0b11,
    EXTS(Seg, Irang2) = 0b00,
    EXTSR(Seg, Irang2) = 0b10,
}

impl AtEx {
    read_fn!(:pub [byte] -> C<Self> {
        C(if byte >> 7 == 1 {
            Self::EXTR(Irang2(byte >> 4 & 0b11))
        } else {
            Self::Atomic(Irang2(byte >> 4))
        })
    });

    write_fn!(:pub self => {
        [match self {
            AtEx::EXTR(irang2) => irang2.0 << 4 | 0b10000000,
            AtEx::Atomic(irang2) => irang2.0 << 4,
        }]
    });
}

impl EXTSeq {
    read_fn!(:pub [b1,b2,b3] -> C<Self> {
        let mode = b1 >> 6;
        let irang = Irang2((b1 >> 4) & 0b11);
        C(if mode & 0b01 == 1 {
            let pag = Pag10(((b2 as u16) << 2) | b3 as u16);
            if mode & 0b10 == 0b10 {
                Self::EXTPR(pag, irang)
            } else {
                Self::EXTP(pag, irang)
            }
        } else {
            let seg = Seg(b2);
            if mode & 0b10 == 0b10 {
                Self::EXTSR(seg, irang)
            } else {
                Self::EXTS(seg, irang)
            }
        })
    });
    write_fn!(:pub self => {
        match self {
            EXTSeq::EXTP(pag10, irang2) => [0b01 << 6 | irang2.0 << 4, (pag10.0 >> 2) as u8, (pag10.0 & 0b11) as u8],
            EXTSeq::EXTPR(pag10, irang2) => [0b11 << 6 | irang2.0 << 4, (pag10.0 >> 2) as u8, (pag10.0 & 0b11) as u8],
            EXTSeq::EXTS(seg8, irang2) => [0b00 << 6 | irang2.0 << 4, seg8.0, 0],
            EXTSeq::EXTSR(seg8, irang2) => [0b10 << 6 | irang2.0 << 4, seg8.0, 0],
        }
    });
}
impl EXTRSeq {
    read_fn!(:pub [byte] -> C<Self> {
        let mode = byte >> 6;
        let reg = GPR(byte & 0x0F);
        let irang = Irang2((byte >> 4) & 0b11);

        C(match mode {
            0b11 => Self::EXTPR(reg, irang),
            0b10 => Self::EXTSR(reg, irang),
            0b01 => Self::EXTP(reg, irang),
            0b00 => Self::EXTS(reg, irang),
            _ => panic!(),
        })
    });

    write_fn!(:pub self => {
        match self {
            EXTRSeq::EXTP(reg, irang2) => [0b01 << 6 | irang2.0 << 4 | reg.0],
            EXTRSeq::EXTPR(reg, irang2) => [0b11 << 6 | irang2.0 << 4 | reg.0],
            EXTRSeq::EXTS(reg, irang2) => [0b00 << 6 | irang2.0 << 4 | reg.0],
            EXTRSeq::EXTSR(reg, irang2) => [0b10 << 6 | irang2.0 << 4 | reg.0],
        }
    });
}

macro_rules! try_from_addr {
    ($t:ident, $($arg:ident),*) => {
        try_from_addr!(value: $t -> {
            if let Address::$t($($arg),*) = value {
                Ok(Self($($arg),*))
            } else {
                Err(())
            }
        });};
    ($value:ident: $t:ident -> $code:block) => {
        impl TryFrom<Address> for $t {
            type Error = ();

            fn try_from($value: Address) -> Result<Self, Self::Error> $code
        }

    };
}

try_from_addr!(Indirect16, a, b);
try_from_addr!(Bitaddr, a, b);

try_from_addr!(val: AtEx -> {
    if let Address::AtEx (v) = val {
        Ok(v)
    } else {
        Err(())
    }
});
try_from_addr!(val: EXTRSeq -> {
    if let Address::EXTRSeq(v) = val {
        Ok(v)
    } else {
        Err(())
    }
});
try_from_addr!(val: EXTSeq -> {
    if let Address::EXTSeq(v) = val {
        Ok(v)
    } else {
        Err(())
    }
});
try_from_addr!(val: ConditionCode -> {
    if let Address::CC(v) = val {
        Ok(v)
    } else {
        Err(())
    }
});

impl From<AtEx> for Address {
    fn from(value: AtEx) -> Self {
        Self::AtEx(value)
    }
}
impl From<Bitaddr> for Address {
    fn from(value: Bitaddr) -> Self {
        Self::Bitaddr(value.0, value.1)
    }
}
impl From<EXTRSeq> for Address {
    fn from(value: EXTRSeq) -> Self {
        Self::EXTRSeq(value)
    }
}
impl From<ConditionCode> for Address {
    fn from(value: ConditionCode) -> Self {
        Self::CC(value)
    }
}
impl From<EXTSeq> for Address {
    fn from(value: EXTSeq) -> Self {
        Self::EXTSeq(value)
    }
}

impl From<Indirect16> for Address {
    fn from(value: Indirect16) -> Self {
        Self::Indirect16(value.0, value.1)
    }
}

impl Address {
    pub fn is_constant(&self) -> bool {
        match self {
            Address::Special(s) => s & 0b1000 == 0,

            Address::Caddr(_)
            | Address::Rel(_)
            | Address::Mask8(_)
            | Address::Pag10(_)
            | Address::Irang2(_)
            | Address::Seg(_)
            | Address::Data16(_)
            | Address::Data8(_)
            | Address::Data4(_)
            | Address::Data3(_) => true,

            _ => false,
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, strum::FromRepr, Eq, PartialEq)]
pub enum ConditionCode {
    Unconditional = 0x0,
    ZoEQ = 0x2,
    NZoNE = 0x3,
    V = 0x4,
    NV = 0x5,
    N = 0x6,
    NN = 0x7,
    CoULT = 0x8,
    NCoUGE = 0x9,
    ULE = 0xF,
    UGT = 0xE,
    SLT = 0xC,
    SLE = 0xB,
    SGE = 0xD,
    SGT = 0xA,
    NET = 0x1,
}
