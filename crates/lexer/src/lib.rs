#![allow(non_upper_case_globals)]
#![feature(let_chains)]

use std::cell::RefCell;
use std::fmt::Debug;

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

struct State {
    pos: usize,
    read_pos: usize,
}

impl Default for State {
    fn default() -> Self {
        Self {
            pos: 0,
            read_pos: 1,
        }
    }
}

pub struct Lexer {
    state: RefCell<State>,
    bytes: Vec<u8>,
    lns: Vec<usize>,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let state = self.state.borrow();
        write!(
            f,
            "Lexer {{ pos: {}, read_pos: {} }}",
            state.pos, state.read_pos
        )
    }
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        let bytes = src.as_bytes();
        Self {
            state: Default::default(),
            bytes: bytes.to_owned(),
            lns: bytes
                .iter()
                .enumerate()
                .filter_map(|(idx, &ch)| (ch == b'\n').then_some(idx))
                .collect(),
        }
    }

    pub fn next_tok(&self) -> Token<'_> {
        self.consume_ws();
        let tok = match self.byte() {
            Some(&ch) => match ch {
                b'=' => self.read_eq(),
                b'!' => self.read_not(),
                b'.' => self.read_dot(),
                b'#' => self.read_hash(),
                b':' => self.read_colon(),
                b'-' => self.read_minus(),
                b'<' => self.read_angle(Shl, Lt, Le),
                b'>' => self.read_angle(Shr, Gt, Ge),
                b'|' => self.read_bw_or_bool(Or, OrOr),
                b'&' => self.read_bw_or_bool(And, AndAnd),
                b'^' => self.read_bin_op(Caret),
                b'+' => self.read_bin_op(Plus),
                b'%' => self.read_bin_op(Percent),
                b'*' => self.read_comp_bin_op(Star, StarStar),
                b'/' => self.read_comp_bin_op(Slash, SlashSlash),
                b'\'' | b'"' => todo!("read string literals"),
                b'@' | b',' | b'~' | b';' | b'{' | b'}' | b'[' | b']' | b'(' | b')' => {
                    let start = self.pos();
                    match ch {
                        b'@' => self.build_tok(start, At),
                        b',' => self.build_tok(start, Comma),
                        b'~' => self.build_tok(start, Tilde),
                        b';' => self.build_tok(start, Semicolon),
                        b'{' => self.build_tok(start, OpenDel(Brace)),
                        b'}' => self.build_tok(start, CloseDel(Brace)),
                        b'[' => self.build_tok(start, OpenDel(Bracket)),
                        b']' => self.build_tok(start, CloseDel(Bracket)),
                        b'(' => self.build_tok(start, OpenDel(Parenthesis)),
                        b')' => self.build_tok(start, CloseDel(Parenthesis)),
                        _ => unreachable!(),
                    }
                }
                _ => {
                    if ch.is_ascii_alphabetic() || ch == b'_' {
                        return self.read_ident_or_kw();
                    } else if ch.is_ascii_digit() {
                        return self.read_number();
                    } else {
                        todo!("global literal errors");
                    }
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
                let end_pos = self.pos();

                let start_row = rows
                    .find_map(|(row, &ln)| (ln > start_pos).then_some(row))
                    .unwrap_or(0);
                let end_row = rows
                    .find_map(|(row, &ln)| (ln > end_pos).then_some(row - 1))
                    .unwrap_or(start_row);

                let (start_col, end_col) = if start_row > 0 {
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
        }; // TODO: Check span determination

        Token::new(kind, span)
    }

    #[inline(always)]
    fn byte(&self) -> Option<&u8> {
        self.bytes.get(self.state.borrow().pos)
    }

    #[inline(always)]
    fn peek_next(&self) -> Option<&u8> {
        self.bytes.get(self.state.borrow().read_pos)
    }

    #[inline(always)]
    fn peek_n(&self, n: usize) -> Option<&u8> {
        self.bytes.get(self.state.borrow().pos + n)
    }

    #[inline(always)]
    fn pos(&self) -> usize {
        self.state.borrow().pos
    }

    #[inline]
    fn adv_pos(&self) {
        let mut state = self.state.borrow_mut();
        state.pos += 1;
        state.read_pos += 1;
    }

    #[inline(always)]
    fn consume_ws(&self) {
        while self.byte().is_some_and(|&ch| ch.is_ascii_whitespace()) {
            self.adv_pos();
        }
    }

    fn read_n_and<'a>(&'a self, n: usize, kind: TokenKind<'a>) -> Token<'a> {
        let start = self.pos();
        (0..n).for_each(|_| self.adv_pos());
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
            Some(ch) if ch == self.byte().unwrap() => self.read_n_and(1, bool),
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

    #[inline(always)]
    fn read_hash(&self) -> Token<'_> {
        self.adv_pos();
        if let Some(&b) = self.byte() && b == b'*' {
            self.adv_pos();
            self.read_while_and(
                |b| b != b'*',
                |s, l| match self.peek_next() {
                    Some(b'#') => {
                        self.adv_pos();
                        self.build_tok(s, Comment(Block, Symbol::new(&self.bytes[s..l - 1])))
                    },
                    _ => {
                        self.build_tok(
                            s,
                            TokenKind::lit(Error, Symbol::new(&self.bytes[s - 2..l + 1]))
                        )
                    },
                }
            )
        } else {
            self.read_while_and(
                |b| b != b'\n',
                |s, l| self.build_tok(s, Comment(Inline, Symbol::new(&self.bytes[s..l])))
            )
        }
    }

    #[inline(always)]
    fn read_ident_or_kw(&self) -> Token<'_> {
        self.read_while_and(
            |b| b.is_ascii_alphabetic() || b == b'_',
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: &str = "
    # this is a boolean value
    var my_bool = true;

    # a float
    const l = 13.5; # yeah, its definetively a float

    #*
        This is a function that performs some math
    *#
    pub func do_math(n) {
        var a = 4;
        a //= 23;

        if n % 2 == 0 {
            return (n + 1) * 2;
        }

        ~a <<= 2;
        a
    }";

    #[test]
    fn test_tokenization() {
        let lexer = Lexer::new(INPUT);
        let mut toks: Vec<Token<'_>> = Vec::new();

        loop {
            let tok = lexer.next_tok();
            toks.push(tok);
            if tok.kind == Eof {
                break;
            }
        }

        dbg!(&toks);
        assert!(toks.len() >= 1);
    }
}
