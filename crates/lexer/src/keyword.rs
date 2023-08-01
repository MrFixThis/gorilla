//! The reserved `keywords` by the language.

macro_rules! keyword {
    { $( $( #$doc:tt )? $name:ident: $val:expr ),+ $(,)* } => {
        $(
            $( #[doc = $doc] )?
            pub const $name: &[u8] = $val.as_bytes();
        )+

        /// Checks if a given literal *symbol* refers to a reserved `keyword`.
        pub fn is_keyword(src: &[u8]) -> bool {
            match src {
                $( $name => true, )+
                _ => false,
            }
        }
    };
}

keyword! {
    Var: "var",
    Const: "const",
    True: "true",
    False: "false",
    If: "if",
    Else: "else",
    Elseif: "elseif",
    For: "for",
    While: "while",
    Loop: "loop",
    Break: "break",
    Continue: "continue",
    Match: "match",
    Enum: "enum",
    Func: "func",
    Return: "return",
    Pack: "pack",
    Pub: "pub",
    As: "as",
}
