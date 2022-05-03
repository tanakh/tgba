macro_rules! trait_alias {
    (pub trait $name:ident = $($traits:tt)+) => {
        pub trait $name: $($traits)* {}
        impl<T: $($traits)*> $name for T {}
    };
}

pub(crate) use trait_alias;
