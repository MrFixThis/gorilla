//! The reserved `keywords` by the language.

macro_rules! keyword {
    { $( $( #$doc:tt )? $name:ident: $val:expr ),+ } => {
        $(
            $( #[doc = $doc] )?
            pub const $name: &str = $val;
        )+

        /// Checks if a given literal *symbol* refers to a reserved `keyword`.
        pub fn is_keyword(src: &str) -> bool {
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
    Pub: "pub",
    As: "as",
    Type: "type"
}
