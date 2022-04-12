use crate::interpreter::Value;

macro_rules! builtin_funcs {
    {$(fn $name:ident $arg:ident { $code:block })*} => {
        $(
            pub fn $name($arg: &Value) -> Value {
                $code
            }
        )*

        #[derive(Debug, Clone, Copy, PartialEq)]
        pub enum Builtin {
            $(
                $name,
            )*
        }

        pub fn run_builtin(builtin: Builtin, arg: &Value) -> Value {
            match builtin {
                $(
                    Builtin::$name => $name(arg),
                )*
            }
        }
    };
}

builtin_funcs! {}
