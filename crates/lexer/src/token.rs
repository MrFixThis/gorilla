use std::borrow::Borrow;
use std::fmt::Debug;
use std::ops::Deref;

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Span {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
}

#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Symbol<'a>(&'a [u8]);

impl Debug for Symbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Borrow::<str>::borrow(self))
    }
}

impl Borrow<str> for Symbol<'_> {
    fn borrow(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.0) }
    }
}

impl Deref for Symbol<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> Symbol<'a> {
    #[inline]
    pub fn new(bytes: &'a [u8]) -> Symbol<'a> {
        Self(bytes)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LitKind {
    Int,
    Float,
    Str,
    Error,
}

#[derive(Clone, Copy, PartialEq)]
pub struct Lit<'a> {
    kind: LitKind,
    symbol: Symbol<'a>,
}

impl Debug for Lit<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Lit { kind, symbol } = self;
        match kind {
            LitKind::Str => write!(f, "{kind:?}, \"{symbol:?}\""),
            _ => write!(f, "{kind:?}, {symbol:?}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CommentKind {
    Inline,
    Block,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DelimiterToken {
    Parenthesis,
    Brace,
    Bracket,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOpToken {
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

#[derive(Clone, Copy, Debug, PartialEq)]
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
    BinOp(BinOpToken),
    BinOpEq(BinOpToken),

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
    OpenDel(DelimiterToken),
    CloseDel(DelimiterToken),
    Ident(Symbol<'a>),

    // Comment symbols
    // # | #*
    Comment(CommentKind, Symbol<'a>),

    // Literals
    // i.e true | 5
    Literal(Lit<'a>),

    // Keywords
    Keyword(Symbol<'a>),

    // Marker symbol
    Eof,
}

impl<'a> TokenKind<'a> {
    #[inline(always)]
    pub fn lit(kind: LitKind, symbol: Symbol<'a>) -> Self {
        Self::Literal(Lit { kind, symbol })
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Span {
            start_row,
            start_col,
            end_row,
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
    #[inline(always)]
    pub fn new(kind: TokenKind<'a>, span: Span) -> Token<'a> {
        Self { kind, span }
    }
}
