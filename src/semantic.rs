mod typing;
mod variable_usage;

use std::collections::HashMap;

pub use typing::*;
pub use variable_usage::*;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref PRELUDE: HashMap<String, Type> = build_prelude();
}

fn build_prelude() -> HashMap<String, Type> {
    [
        ("almost_pi", Type::Int),
        (
            "print",
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        ),
        (
            "println",
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        ),
        ("inc", Type::Function(vec![Type::Int], Box::new(Type::Int))),
    ]
    .map(|(name, ty)| (name.to_string(), ty))
    .into_iter()
    .collect()
}
