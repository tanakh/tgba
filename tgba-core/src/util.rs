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
