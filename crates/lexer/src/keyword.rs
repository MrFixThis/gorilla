macro_rules! keyword {
    { $( $( #$doc:tt )? $name:ident: $val:expr ),+ $(,)* } => {
        /// Reserved `keywords`.
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub enum Keyword {
            $(
                $( #[doc = $doc] )?
                $name,
            )*
        }

        impl Keyword {
            pub fn try_from_bytes(src: &[u8]) -> Option<Self> {
                match unsafe { std::str::from_utf8_unchecked(src) } { // this may panic, okay?
                    $( $val => Some(Self::$name), )+
                    _ => None,
                }
            }

            pub fn as_str(&self) -> &str {
                match self {
                    $( Self::$name => $val, )+
                }
            }
        }
    };
}

keyword! {
    Nil: "nil",
    True: "true",
    False: "false",
    Var: "var",
    Const: "const",
    If: "if",
    Else: "else",
    Elseif: "elseif",
    For: "for",
    While: "while",
    Loop: "loop",
    Break: "break",
    Continue: "continue",
    Func: "func",
    Return: "return",
    Include: "include",
    Pub: "pub",
    Priv: "priv",
}
