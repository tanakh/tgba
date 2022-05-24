macro_rules! trait_alias {
    (pub trait $name:ident = $($traits:tt)+) => {
        pub trait $name: $($traits)* {}
        impl<T: $($traits)*> $name for T {}
    };
}

pub(crate) use trait_alias;

macro_rules! pack {
    (@packing $view:ident $x:literal..=$y:literal => $v:expr $(, $($rest:tt)*)?) => {
        $view[$x..=$y].store($v);
        pack!(@packing $view $($($rest)*)*);
    };
    (@packing $view:ident $x:literal => $v:expr $(, $($rest:tt)*)?) => {
        $view.set($x, $v);
        pack!(@packing $view $($($rest)*)*);
    };
    (@packing $view:ident $(,)?) => {};
    (@packing $($rest:tt)*) => {
        compile_error!("Invalid input for macro pack!");
    };
    ($($input:tt)*) => {{
        use bitvec::prelude::*;
        let mut data = 0;
        let view = data.view_bits_mut::<bitvec::prelude::Lsb0>();
        pack!(@packing view $($input)*);
        data
    }};
}

pub(crate) use pack;

pub fn read16(p: &[u8], addr: usize) -> u16 {
    u16::from_le_bytes(p[addr..addr + 2].try_into().unwrap())
}

pub fn read32(p: &[u8], addr: usize) -> u32 {
    u32::from_le_bytes(p[addr..addr + 4].try_into().unwrap())
}

pub fn write16(p: &mut [u8], addr: usize, data: u16) {
    p[addr..addr + 2].copy_from_slice(&data.to_le_bytes());
}

pub fn write32(p: &mut [u8], addr: usize, data: u32) {
    p[addr..addr + 4].copy_from_slice(&data.to_le_bytes());
}

pub struct ConstEval<const V: u8>;

impl<const V: u8> ConstEval<V> {
    pub const VALUE: u8 = V;
}

macro_rules! enum_pat {
    ($e:expr) => {
        ConstEval::<{ $e as u8 }>::VALUE
    };
}
pub(crate) use enum_pat;
