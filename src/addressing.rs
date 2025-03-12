macro_rules! read {
    ($reader: ident, $n:expr) => {{
        let mut b = [0u8; $n];
        $reader.read_exact(&mut b).unwrap();
        b
    }};
}

macro_rules! mk_address {
    {
        $($name:ident($type:ty)),*
    } => {
        #[derive(Debug, Copy, Clone)]
        pub enum Address {$(
           $name($type),
        )*
            Bitaddr(u8, u8),
            Indirect16(u8, u16),
        }

        $(
        #[derive(Debug, Copy, Clone)]
        pub struct $name(pub $type);

        impl From<$name> for Address {
            fn from(value: $name) -> Self {
                Self::$name(value.0)
            }
        }
        )*
    };
}

macro_rules! readable {
    ($a:ident, $reader:ident => $code:block) => {
        impl $a {
            pub fn read<R: std::io::Read>($reader: &mut R) -> C<$a> $code
        }
    };
    (<$a:ident, $b:ident>, $reader:ident => $code:block) => {
        impl ArgRead for ($a, $b) {
            fn read<R: std::io::Read>($reader: &mut R) -> Self $code
        }
    };

    (4_4: $a:ident, $b:ident) => {
        readable!(<$a, $b>, reader => {
            let [byte] = read!(reader, 1);

            ($a(byte >> 4), $b(byte & 0x0F))
        });
    };

    (4_16: $a:ident, $b:ident) => {
        readable!(<$a, $b>, reader => {
            let b = read!(reader, 3);

            ($a(b[0] & 0x0F), $b(u16::from_le_bytes([b[1],b[2]])))
        });
    };

    (8_16: $a:ident, $b:ident) => {
        readable!(<$a, $b>, reader => {
            let b = read!(reader, 3);

            ($a(b[0]), $b(u16::from_le_bytes([b[1], b[2]])))
        });
    };
    (!8_16: $b:ident, $a:ident) => {
        impl ArgRead for ($a, $b) {
            fn read<R: std::io::Read>(reader: &mut R) -> Self {
                let [b1,b2,b3] = read!(reader, 3);
                ($a(u16::from_le_bytes([b2, b3])), $b(b1))
            }
        }
    };

    (8_8: $a:ident, $b:ident) => {
        readable!(<$a, $b>, reader => {
            let [b1,b2,_] = read!(reader, 3);

            ($a(b1), $b(b2))
        });
    };

    (8(4)x2: $a:ident, $b:ident) => {
        readable!(<$a, $b>, reader => {
            let [b1,b2,b3] = read!(reader, 3);

            ($a(b1, b3 >> 4), $b(b2, b3 & 0x0F))
        });
    };

    (40: $a:ident) => {
        readable!($a, reader => {
            let [byte] = read!(reader, 1);

            C($a(byte >> 4))
        });
    };
    (8: $a:ident) => {
        readable!($a, reader => {
            let [byte] = read!(reader, 1);

            C($a(byte))
        });
    };
}

macro_rules! read_44 {
    ($($t:ident),*) => {
    $(
        readable!(4_4: GPR, $t);
        readable!(4_4: $t, GPR);
        readable!(4_4: GPRb, $t);
        readable!(4_4: $t, GPRb);
        readable!(4_4: Indirect, $t);
    )*
    };
}

mk_address! {
    GPR(u8),
    GPRb(u8),
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
    Seg8(u8),
    Irang2(u8),
    Rel(u8),
    Caddr(u16),
    Trap7(u8),
    Seg(u8),
    EINITs(())
}

read_44!(Data4, Special, Indirect, IndirectDecr, IndirectIncr);

readable!(4_4: GPR, GPR);
readable!(4_4: GPR, GPRb);
readable!(4_4: GPRb, GPRb);
readable!(4_16: GPR, Mem);
readable!(4_16: GPR, Data16);
readable!(8_8: Reg, Data8);
readable!(8_16: Reg, Mem);
readable!(8_16: Seg, Caddr);
readable!(8_16: Reg, Data16);
readable!(!8_16: Reg, Mem);
readable!(8_16: Indirect, Mem);
readable!(!8_16: Indirect, Mem);
readable!(8(4)x2: Bitaddr, Bitaddr);
readable!(8: GPR);
readable!(8: GPRb);
readable!(8: Bitoff);
readable!(8: Rel);
readable!(8: Reg);
readable!(8: Trap7);

macro_rules! read_fn {
    ($(:$p:vis)? [$($n:ident),*] -> $r:ty $code:block) => {
        $($p)? fn read<R: std::io::Read>(reader: &mut R) -> $r {
            let [$($n),*] = read!(reader, ${count($n)});

            $code
        }
    };
}

pub trait ArgRead {
    fn read<R: std::io::Read>(reader: &mut R) -> Self;
}

impl ArgRead for () {
    read_fn!([_e] -> () {()});
}

impl EINITs {
    read_fn!(:pub [a,b,c] -> C<EINITs> {
        assert_eq!([a,b,c],[0x4A,0xB5,0xB5]);
        C(EINITs(()))
    });
}

impl Irang2 {
    read_fn!(:pub [byte] -> C<Self> {
        C(Irang2((byte & 0b00110000) >> 4))
    });
}

impl ArgRead for (Indirect16, GPR) {
    read_fn!([b1,b2,b3] -> Self {(
        Indirect16(b1 & 0x0F, u16::from_le_bytes([b2, b3])),
        GPR(b1 >> 4),
    )});
}
impl ArgRead for (Indirect16, GPRb) {
    read_fn!([b1,b2,b3] -> Self {(
        Indirect16(b1 & 0x0F, u16::from_le_bytes([b2, b3])),
        GPRb(b1 >> 4),
    )});
}
impl ArgRead for (GPR, Indirect16) {
    read_fn!([b1,b2,b3] -> Self {(
        GPR(b1 >> 4),
        Indirect16(b1 & 0x0F, u16::from_le_bytes([b2, b3])),
    )});
}
impl ArgRead for (GPRb, Indirect16) {
    read_fn!([b1,b2,b3] -> Self {(
        GPRb(b1 >> 4),
        Indirect16(b1 & 0x0F, u16::from_le_bytes([b2, b3])),
    )});
}

impl ArgRead for (Bitoff, Mask8, Data8) {
    read_fn!([b1,b2,b3] -> Self {
        (Bitoff(b1), Mask8(b2), Data8(b3))
    });
}

impl ArgRead for (ConditionCode, IndirectIncr) {
    read_fn!([byte] -> Self {(
        ConditionCode::from_repr(byte >> 4).unwrap(),
        IndirectIncr(byte & 0x0F),
    )});
}
impl ArgRead for (ConditionCode, Indirect) {
    read_fn!([byte] -> Self {(
        ConditionCode::from_repr(byte >> 4).unwrap(),
        Indirect(byte & 0x0F),
    )});
}
impl ArgRead for (ConditionCode, Caddr) {
    read_fn!([b1, b2, b3] -> Self {(
        ConditionCode::from_repr(b1 >> 4).unwrap(),
        Caddr(u16::from_le_bytes([b2, b3])),
    )});
}
impl ArgRead for (Bitaddr, Rel) {
    read_fn!([b1,b2,b3] -> Self {
        (Bitaddr(b1, b3 >> 4), Rel(b2))
    });
}

pub struct C<T>(pub T);

#[derive(Debug, Clone, Copy)]
pub struct Bitaddr(pub u8, pub u8);
#[derive(Debug, Clone, Copy)]
pub struct Indirect16(pub u8, pub u16);

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum EXTRSeq {
    EXTP(GPR, Irang2) = 0b01,
    EXTPR(GPR, Irang2) = 0b11,
    EXTS(GPR, Irang2) = 0b00,
    EXTSR(GPR, Irang2) = 0b10,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum EXTSeq {
    EXTP(Pag10, Irang2) = 0b01,
    EXTPR(Pag10, Irang2) = 0b11,
    EXTS(Seg8, Irang2) = 0b00,
    EXTSR(Seg8, Irang2) = 0b10,
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
            let seg = Seg8(b2);
            if mode & 0b10 == 0b10 {
                Self::EXTSR(seg, irang)
            } else {
                Self::EXTS(seg, irang)
            }
        })
    });
}
impl EXTRSeq {
    read_fn!(:pub [byte] -> C<Self> {
        let mode = byte >> 6;
        let reg = GPR(byte & 0x0F);
        let irang = Irang2((byte >> 6) & 0b11);

        C(match mode {
            0b11 => Self::EXTPR(reg, irang),
            0b10 => Self::EXTSR(reg, irang),
            0b01 => Self::EXTP(reg, irang),
            0b00 => Self::EXTS(reg, irang),
            _ => panic!(),
        })
    });
}

impl From<Bitaddr> for Address {
    fn from(value: Bitaddr) -> Self {
        Self::Bitaddr(value.0, value.1)
    }
}

impl From<Indirect16> for Address {
    fn from(value: Indirect16) -> Self {
        Self::Indirect16(value.0, value.1)
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, strum::FromRepr)]
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
