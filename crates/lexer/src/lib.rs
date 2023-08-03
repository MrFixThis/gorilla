#![allow(non_upper_case_globals)]
#![feature(let_chains)]
#![feature(cell_update)]

use std::cell::Cell;
use std::fmt::Debug;

#[cfg(test)]
mod tests;

pub mod keyword;
pub mod token;

use token::BinOpToken::{self, *};
use token::CommentKind::*;
use token::DelimiterToken::*;
use token::LitKind::*;
use token::Span;
use token::Symbol;
use token::Token;
use token::TokenKind::{self, *};

pub struct Lexer {
    pos: Cell<usize>,
    bytes: Vec<u8>,
    lns: Vec<usize>,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pos = self.pos.get();
        write!(f, "Lexer {{ pos: {}, read_pos: {} }}", pos, pos + 1)
    }
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        let bytes = src.as_bytes();
        Self {
            pos: Default::default(),
            bytes: bytes.to_owned(),
            lns: bytes
                .iter()
                .enumerate()
                .filter_map(|(idx, &b)| (b == b'\n').then_some(idx))
                .collect(),
        }
    }

    pub fn next_tok(&self) -> Token<'_> {
        self.consume_ws();
        let tok = match self.byte() {
            Some(&b) => match b {
                b'=' => self.read_eq(),
                b'!' => self.read_not(),
                b'.' => self.read_dot(),
                b'#' => self.read_hash(),
                b':' => self.read_colon(),
                b'-' => self.read_minus(),
                b'\'' | b'"' => self.read_str(),
                b'<' => self.read_angle(Shl, Lt, Le),
                b'>' => self.read_angle(Shr, Gt, Ge),
                b'|' => self.read_bw_or_bool(Or, OrOr),
                b'&' => self.read_bw_or_bool(And, AndAnd),
                b'^' => self.read_bin_op(Caret),
                b'+' => self.read_bin_op(Plus),
                b'%' => self.read_bin_op(Percent),
                b'*' => self.read_comp_bin_op(Star, StarStar),
                b'/' => self.read_comp_bin_op(Slash, SlashSlash),
                b'@' => self.build_tok(self.pos(), At),
                b',' => self.build_tok(self.pos(), Comma),
                b'~' => self.build_tok(self.pos(), Tilde),
                b';' => self.build_tok(self.pos(), Semicolon),
                b'{' => self.build_tok(self.pos(), OpenDel(Brace)),
                b'}' => self.build_tok(self.pos(), CloseDel(Brace)),
                b'[' => self.build_tok(self.pos(), OpenDel(Bracket)),
                b']' => self.build_tok(self.pos(), CloseDel(Bracket)),
                b'(' => self.build_tok(self.pos(), OpenDel(Parenthesis)),
                b')' => self.build_tok(self.pos(), CloseDel(Parenthesis)),
                r if r.is_ascii_digit() => return self.read_number(),
                r if r.is_ascii_alphabetic() || r == b'_' => return self.read_ident_or_kw(),
                _ => {
                    let pos = self.pos();
                    self.build_tok(
                        pos,
                        TokenKind::lit(Error, Symbol::new(&self.bytes[pos..=pos])),
                    )
                }
            },
            None => return self.build_tok(0, Eof),
        };
        self.adv_pos();
        tok
    }

    fn build_tok<'a>(&'a self, start_pos: usize, kind: TokenKind<'a>) -> Token<'a> {
        let span = match kind {
            Eof => Default::default(),
            _ => {
                let mut rows = self.lns.iter().enumerate();
                let end_pos = {
                    let pos = self.pos();
                    if start_pos == pos {
                        pos
                    } else {
                        pos - 1
                    }
                };

                let start_row = rows
                    .find_map(|(row, &ln)| (ln > start_pos).then_some(row))
                    .unwrap_or(0);
                let end_row = rows
                    .find_map(|(row, &ln)| (ln > end_pos).then_some(row - 1))
                    .unwrap_or(start_row);

                let (start_col, end_col) = if start_row > 0 && end_row >= start_row {
                    (
                        start_pos - self.lns[start_row - 1] - 1,
                        end_pos - self.lns[end_row - 1] - 1,
                    )
                } else {
                    (start_pos, end_pos)
                };

                Span {
                    start_row,
                    start_col,
                    end_row,
                    end_col,
                }
            }
        };

        Token::new(kind, span)
    } // TODO: Check span determination

    #[inline(always)]
    fn byte(&self) -> Option<&u8> {
        self.bytes.get(self.pos.get())
    }

    #[inline(always)]
    fn peek_next(&self) -> Option<&u8> {
        self.bytes.get(self.pos.get() + 1)
    }

    #[inline(always)]
    fn peek_n(&self, n: usize) -> Option<&u8> {
        self.bytes.get(self.pos.get() + n)
    }

    #[inline(always)]
    fn pos(&self) -> usize {
        self.pos.get()
    }

    #[inline(always)]
    fn adv_pos(&self) {
        _ = self.pos.update(|p| p + 1)
    }

    #[inline(always)]
    fn consume_ws(&self) {
        while self.byte().is_some_and(|&b| b.is_ascii_whitespace()) {
            self.adv_pos();
        }
    }

    fn read_n_and<'a>(&'a self, n: usize, kind: TokenKind<'a>) -> Token<'a> {
        let start = self.pos();
        (0..=n).for_each(|_| self.adv_pos());
        self.build_tok(start, kind)
    }

    fn read_while_and<'a, P, R>(&'a self, p: P, r: R) -> Token<'a>
    where
        P: Fn(u8) -> bool,
        R: FnOnce(usize, usize) -> Token<'a>,
    {
        let start_pos = self.pos();
        while let Some(&b) = self.byte() && p(b) {
            self.adv_pos();
        }
        r(start_pos, self.pos())
    }

    // +=============+
    // || SPECIFICS ||
    // +=============+

    #[inline]
    fn read_minus(&self) -> Token<'_> {
        match self.peek_next() {
            Some(b'>') => self.read_n_and(1, Arrow),
            Some(b'=') => self.read_n_and(1, BinOpEq(Minus)),
            _ => self.build_tok(self.pos(), BinOp(Minus)),
        }
    }

    #[inline]
    fn read_not(&self) -> Token<'_> {
        match self.peek_next() {
            Some(b'=') => self.read_n_and(1, Ne),
            _ => self.build_tok(self.pos(), Not),
        }
    }

    #[inline]
    fn read_dot(&self) -> Token<'_> {
        match (self.peek_next(), self.peek_n(2)) {
            (Some(b'.'), Some(b'.')) => self.read_n_and(2, Ellipsis),
            (Some(b'.'), _) => self.read_n_and(1, DotDot),
            _ => self.build_tok(self.pos(), Dot),
        }
    }

    #[inline]
    fn read_eq(&self) -> Token<'_> {
        match self.peek_next() {
            Some(b'=') => self.read_n_and(1, EqEq),
            Some(b'>') => self.read_n_and(1, FatArrow),
            _ => self.build_tok(self.pos(), Eq),
        }
    }

    #[inline]
    fn read_colon(&self) -> Token<'_> {
        match self.peek_next() {
            Some(b':') => self.read_n_and(1, ColonColon),
            _ => self.build_tok(self.pos(), Colon),
        }
    }

    fn read_bw_or_bool<'a>(&'a self, bw: BinOpToken, bool: TokenKind<'a>) -> Token<'a> {
        match self.peek_next() {
            Some(b) if b == self.byte().unwrap() => self.read_n_and(1, bool),
            Some(b'=') => self.read_n_and(1, BinOpEq(bw)),
            _ => self.build_tok(self.pos(), BinOp(bw)),
        }
    }

    fn read_bin_op(&self, bin_op: BinOpToken) -> Token<'_> {
        match self.peek_next() {
            Some(b'=') => self.read_n_and(1, BinOpEq(bin_op)),
            _ => self.build_tok(self.pos(), BinOp(bin_op)),
        }
    }

    fn read_comp_bin_op(&self, sing_op: BinOpToken, comp_op: BinOpToken) -> Token<'_> {
        match self.peek_next() {
            Some(n) if n == self.byte().unwrap() => match self.peek_n(2) {
                Some(b'=') => self.read_n_and(2, BinOpEq(comp_op)),
                _ => self.read_n_and(1, BinOp(comp_op)),
            },
            Some(b'=') => self.read_n_and(1, BinOpEq(sing_op)),
            _ => self.build_tok(self.pos(), BinOp(sing_op)),
        }
    }

    // HACK: Eliminate boilerplate ^^
    fn read_angle<'a>(
        &'a self,
        bw: BinOpToken,
        bool: TokenKind<'a>,
        bool_eq: TokenKind<'a>,
    ) -> Token<'a> {
        match self.peek_next() {
            Some(n) if n == self.byte().unwrap() => match self.peek_n(2) {
                Some(b'=') => self.read_n_and(2, BinOpEq(bw)),
                _ => self.build_tok(self.pos(), BinOp(bw)),
            },
            Some(b'=') => self.read_n_and(1, bool_eq),
            _ => self.build_tok(self.pos(), bool),
        }
    }

    #[inline]
    fn read_hash(&self) -> Token<'_> {
        let org_pos = self.pos();
        self.adv_pos();

        if let Some(&b) = self.byte() && b == b'*' {
            self.adv_pos();
            self.read_while_and(
                |b| b != b'*',
                |s, l| match self.peek_next() {
                    Some(b'#') => {
                        self.adv_pos();
                        self.build_tok(org_pos, Comment(Block, Symbol::new(&self.bytes[s..l - 1])))
                    },
                    _ => {
                        self.build_tok(
                            org_pos,
                            TokenKind::lit(Error, Symbol::new(&self.bytes[org_pos..=l - 1]))
                        )
                    },
                }
            )
        } else {
            self.read_while_and(
                |b| b != b'\n',
                |s, l| self.build_tok(org_pos, Comment(Inline, Symbol::new(&self.bytes[s..l])))
            )
        }
    }

    #[inline(always)]
    fn read_ident_or_kw(&self) -> Token<'_> {
        self.read_while_and(
            |b| b.is_ascii_alphanumeric() || b == b'_',
            |s, l| {
                let sym = Symbol::new(&self.bytes[s..l]);
                let kind = if keyword::is_keyword(&sym) {
                    Keyword(sym)
                } else {
                    Ident(sym)
                };
                self.build_tok(s, kind)
            },
        )
    }

    #[inline]
    fn read_str(&self) -> Token<'_> {
        let f = *self.byte().unwrap();
        let org_pos = self.pos();
        self.adv_pos();

        self.read_while_and(
            |b| b != b'"' && b != b'\'',
            |s, l| {
                if let Some(&c) = self.byte() && c == f {
                    self.build_tok(org_pos, TokenKind::lit(Str, Symbol::new(&self.bytes[s..l])))
                } else {
                    self.build_tok(
                        org_pos, TokenKind::lit(Error, Symbol::new(&self.bytes[org_pos..=l]))
                    )
                }
            },
        )
    } // TODO: read scaped strings

    #[inline(always)]
    fn read_number(&self) -> Token<'_> {
        self.read_while_and(
            |b| b.is_ascii_digit() || b == b'.',
            |s, l| {
                let (kind, bytes) = {
                    let bytes = &self.bytes[s..l];
                    if bytes.contains(&b'.') {
                        (Float, bytes)
                    } else {
                        (Int, bytes)
                    }
                };
                self.build_tok(s, TokenKind::lit(kind, Symbol::new(bytes)))
            },
        )
    } // TODO: look for multiple periods in the float numbers
}
