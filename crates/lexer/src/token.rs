mod symbol;

use std::fmt::Debug;

use crate::keyword::Keyword;
pub use symbol::Symbol;

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum LiteralKind {
    Int,
    Float,
    Str,
    Error,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Literal<'a> {
    kind: LiteralKind,
    symbol: Symbol<'a>,
}

impl Debug for Literal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Literal { kind, symbol } = self;
        match kind {
            LiteralKind::Str => write!(f, "{kind:?}, \"{symbol:?}\""),
            _ => write!(f, "{kind:?}, {symbol:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CommentKind {
    Inline,
    Block,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BinOperator {
    Plus,
    Minus,
    Star,
    StarStar,
    Slash,
    SlashSlash,
    Percent,
    Or,
    And,
    Caret,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum TokenKind<'a> {
    //Expression-operator symbols
    /*
    Equal                   =
    EqualEqual              ==
    Not equal               !=
    Less than               <
    Less than Eq to         <=
    Greater than            >
    Greater than Eq to      >=
    AndAnd                  &&
    OrOr                    ||
    Not                     !
    Tilde                   ~
    --------------------------
    Plus                    +
    Minus                   -
    Multiplication          *
    Power                   **
    Divison                 /
    Floor divison           //
    Modulus                 %
    Bitwise or              |
    Bitwise and             &
    Bitwise xor             ^
    Left shift              <<
    Right shift             >>
    --------------------------
    Plus Eq                 +=
    Minus Eq                -=
    Multiplication Eq       *=
    Power Eq                **=
    Divison Eq              /=
    Floor divison Eq        //=
    Modulus Eq              %=
    Bitwise or Eq           |=
    Bitwise and Eq          &=
    Bitwise xor Eq          ^=
    Left shift Eq           <<=
    Right shift Eq          >>=
    */
    Eq,
    EqEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    AndAnd,
    OrOr,
    Not,
    Tilde,
    BinOp(BinOperator),
    BinOpEq(BinOperator),

    // Structural symbols
    /*
    At                      @
    Dot                     .
    DotDot                  ..
    Ellipsis                ...
    Comma                   ,
    Colon                   :
    ColonColon              ::
    Semicolon               ;
    Arrow                   ->
    FatArrow                =>
    OpenDel                 i.e {
    CloseDel                i.e }
    Ident                   i.e foo | bar
    */
    At,
    Dot,
    DotDot,
    Ellipsis,
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    Arrow,
    FatArrow,
    OpenDel(Delimiter),
    CloseDel(Delimiter),
    Ident(Symbol<'a>),

    // Comment symbols
    // # | #*
    Comment(CommentKind, Symbol<'a>),

    // Literals
    // i.e true | 5
    Literal(Literal<'a>),

    // Keywords
    Keyword(Keyword),

    // Marker symbol
    Eof,
}

impl<'a> TokenKind<'a> {
    #[inline]
    pub fn lit(kind: LiteralKind, symbol: Symbol<'a>) -> Self {
        Self::Literal(Literal { kind, symbol })
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Span {
            start_line: start_row,
            start_col,
            end_line: end_row,
            end_col,
        } = self.span;
        write!(
            f,
            "{:?} -> [{} - {}, {} - {}]",
            self.kind, start_row, start_col, end_row, end_col,
        )
    }
}

impl<'a> Token<'a> {
    #[inline]
    pub fn new(kind: TokenKind<'a>, span: Span) -> Token<'a> {
        Self { kind, span }
    }
}
