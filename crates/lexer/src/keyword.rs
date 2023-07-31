//! The reserved `keywords` by the language.

macro_rules! keyword {
    { $( $( #$doc:tt )? $name:ident: $val:expr ),+ $(,)* } => {
        $(
            $( #[doc = $doc] )?
            pub const $name: &str = $val;
        )+

        /// Checks if a given literal *symbol* refers to a reserved `keyword`.
        pub fn is_keyword<T: ::std::borrow::Borrow<str>>(src: &T) -> bool {
            match src.borrow() {
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
