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
    chars: Vec<char>,
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
        Self {
            state: Default::default(),
            chars: src.chars().collect(),
            lns: src
                .chars()
                .enumerate()
                .filter_map(|(idx, ch)| (ch == '\n').then_some(idx))
                .collect(),
        }
    }

    pub fn next_tok(&self) -> Token<'_> {
        self.consume_ws();
        let tok = match self.char() {
            Some(&ch) => match ch {
                '=' => self.read_eq(),
                '!' => self.read_not(),
                '.' => self.read_dot(),
                '#' => self.read_hash(),
                ':' => self.read_colon(),
                '-' => self.read_minus(),
                '<' => self.read_angle(Shl, Lt, Le),
                '>' => self.read_angle(Shr, Gt, Ge),
                '|' => self.read_bw_or_bool(Or, OrOr),
                '&' => self.read_bw_or_bool(And, AndAnd),
                '^' => self.read_bin_op(Caret),
                '+' => self.read_bin_op(Plus),
                '%' => self.read_bin_op(Percent),
                '*' => self.read_comp_bin_op(Star, StarStar),
                '/' => self.read_comp_bin_op(Slash, SlashSlash),
                '@' | ',' | '~' | ';' | '{' | '}' | '[' | ']' | '(' | ')' => {
                    let start = self.pos();
                    match ch {
                        '@' => self.build_tok(start, At),
                        ',' => self.build_tok(start, Comma),
                        '~' => self.build_tok(start, Tilde),
                        ';' => self.build_tok(start, Semicolon),
                        '{' => self.build_tok(start, OpenDel(Brace)),
                        '}' => self.build_tok(start, CloseDel(Brace)),
                        '[' => self.build_tok(start, OpenDel(Bracket)),
                        ']' => self.build_tok(start, CloseDel(Bracket)),
                        '(' => self.build_tok(start, OpenDel(Parenthesis)),
                        ')' => self.build_tok(start, CloseDel(Parenthesis)),
                        _ => unreachable!(),
                    }
                }
                _ => {
                    if ch.is_ascii_alphabetic() || ch == '_' || ch == '\'' || ch == '"' {
                        return self.read_ident_or_kw();
                    } else if ch.is_ascii_digit() {
                        return self.read_number();
                    } else {
                        todo!("literal error");
                    }
                }
            },
            None => return self.build_tok(self.pos(), Eof),
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
        };

        Token::new(kind, span)
    }

    #[inline(always)]
    fn char(&self) -> Option<&char> {
        self.chars.get(self.state.borrow().pos)
    }

    #[inline(always)]
    fn peek_next(&self) -> Option<&char> {
        self.chars.get(self.state.borrow().read_pos)
    }

    #[inline(always)]
    fn peek_n(&self, n: usize) -> Option<&char> {
        self.chars.get(self.state.borrow().pos + n)
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

    #[inline]
    fn consume_ws(&self) {
        while self.char().is_some_and(|&ch| ch.is_ascii_whitespace()) {
            self.adv_pos();
        }
    }

    fn read_n<'a>(&'a self, n: usize, kind: TokenKind<'a>) -> Token<'a> {
        let start = self.pos();
        (0..n).for_each(|_| self.adv_pos());
        self.build_tok(start, kind)
    }

    fn read_while_and<'a, P, R>(&'a self, p: P, r: R) -> Token<'a>
    where
        P: Fn(char) -> bool,
        R: FnOnce(usize, usize) -> Token<'a>,
    {
        let start = self.pos();
        while let Some(&c) = self.char() && p(c) {
            self.adv_pos();
        }
        r(start, self.pos() - 1)
    }

    // +=============+
    // || SPECIFICS || HACK: Eliminate boilerplate
    // +=============+

    #[inline]
    fn read_minus(&self) -> Token<'_> {
        match self.peek_next() {
            Some('>') => self.read_n(1, Arrow),
            Some('=') => self.read_n(1, BinOpEq(Minus)),
            _ => self.build_tok(self.pos(), BinOp(Minus)),
        }
    }

    #[inline]
    fn read_not(&self) -> Token<'_> {
        match self.peek_next() {
            Some('=') => self.read_n(1, Ne),
            _ => self.build_tok(self.pos(), Not),
        }
    }

    #[inline]
    fn read_dot(&self) -> Token<'_> {
        match (self.peek_next(), self.peek_n(2)) {
            (Some('.'), Some('.')) => self.read_n(2, Ellipsis),
            (Some('.'), _) => self.read_n(1, DotDot),
            _ => self.build_tok(self.pos(), Dot),
        }
    }

    #[inline]
    fn read_eq(&self) -> Token<'_> {
        match self.peek_next() {
            Some('=') => self.read_n(1, EqEq),
            Some('>') => self.read_n(1, FatArrow),
            _ => self.build_tok(self.pos(), Eq),
        }
    }

    #[inline]
    fn read_colon(&self) -> Token<'_> {
        match self.peek_next() {
            Some(':') => self.read_n(1, ColonColon),
            _ => self.build_tok(self.pos(), Colon),
        }
    }

    fn read_bw_or_bool<'a>(&'a self, bw: BinOpToken, bool: TokenKind<'a>) -> Token<'a> {
        match self.peek_next() {
            Some(ch) if ch == self.char().unwrap() => self.read_n(1, bool),
            Some('=') => self.read_n(1, BinOpEq(bw)),
            _ => self.build_tok(self.pos(), BinOp(bw)),
        }
    }

    fn read_angle<'a>(
        &'a self,
        bw: BinOpToken,
        bool: TokenKind<'a>,
        bool_eq: TokenKind<'a>,
    ) -> Token<'a> {
        match self.peek_next() {
            Some(n) if n == self.char().unwrap() => match self.peek_n(2) {
                Some('=') => self.read_n(2, BinOpEq(bw)),
                _ => self.build_tok(self.pos(), BinOp(bw)),
            },
            Some('=') => self.read_n(1, bool_eq),
            _ => self.build_tok(self.pos(), bool),
        }
    }

    fn read_bin_op(&self, bin_op: BinOpToken) -> Token<'_> {
        match self.peek_next() {
            Some('=') => self.read_n(1, BinOpEq(bin_op)),
            _ => self.build_tok(self.pos(), BinOp(bin_op)),
        }
    }

    fn read_comp_bin_op(&self, sing_op: BinOpToken, comp_op: BinOpToken) -> Token<'_> {
        match self.peek_next() {
            Some(n) if n == self.char().unwrap() => match self.peek_n(2) {
                Some('=') => self.read_n(2, BinOpEq(comp_op)),
                _ => self.read_n(1, BinOp(comp_op)),
            },
            Some('=') => self.read_n(1, BinOpEq(sing_op)),
            _ => self.build_tok(self.pos(), BinOp(sing_op)),
        }
    }

    fn read_hash(&self) -> Token<'_> {
        match self.peek_next() {
            Some('*') => self.read_while_and(
                |c| c != '*',
                |s, l| {
                    self.adv_pos();
                    let sym = Symbol::new(&self.chars[s + 2..=l - 1]);
                    let tok = match self.char() {
                        Some('#') => self.build_tok(s, Comment(Block, sym)),
                        _ => self.build_tok(s, TokenKind::lit(Error, sym)),
                    };
                    tok
                },
            ),
            _ => self.read_while_and(
                |c| c != '\n' || c != '\0',
                |s, l| {
                    self.build_tok(
                        s,
                        Comment(Inline, Symbol::new(&self.chars[s + 1..=l])),
                    )
                },
            ),
        }
    } // TODO: check this

    #[inline]
    fn read_ident_or_kw(&self) -> Token<'_> {
        match self.char() {
            Some(&_f @ '\'' | &_f @ '"') => self.read_while_and( // TODO: Improve condition
                |c| c != '\'' || c != '"',
                |s, l| {
                    self.build_tok(
                        s,
                        TokenKind::lit(Str, Symbol::new(&self.chars[s + 1..=l - 1])),
                    )
                },
            ),
            _ => self.read_while_and(
                |c| c.is_ascii_alphabetic() || c == '_',
                |s, l| {
                    let sym = Symbol::new(&self.chars[s..=l]);
                    let kind = if keyword::is_keyword(&sym) { // TODO: Check corroboration problem
                        TokenKind::Keyword(sym)
                    } else {
                        TokenKind::Ident(sym)
                    };
                    self.build_tok(s, kind)
                },
            ),
        }
    }

    #[inline(always)]
    fn read_number(&self) -> Token<'_> {
        self.read_while_and(
            |c| c.is_ascii_digit() || c == '.',
            |s, l| {
                let (kind, chars) = 'b: {
                    let chars = &self.chars[s..=l];
                    if chars.contains(&'.') {
                        break 'b (Float, chars);
                    }
                    (Int, chars)
                };
                self.build_tok(s, TokenKind::lit(kind, Symbol::new(chars)))
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // static INPUT: &str = "#* BLOCK *#";

    static INPUT: &str = r#""Rust!""#;

    // static INPUT: &str = "
    // var my_bool = true;
    // const l = 13;
    //
    // pub func do_math(n) {
    //     var a = 4;
    //
    //     if n % 2 == 0 {
    //         return (n + 1) * 2;
    //     }
    //
    //     a <<= 2;
    //     a
    // }";

    #[test]
    fn test_non_ident_reading() {
        let lexer = Lexer::new(INPUT);
        let mut toks: Vec<Token<'_>> = Vec::new();

        loop {
            match lexer.next_tok() {
                t @ Token { kind: Eof, .. } => {
                    toks.push(t);
                    break;
                }
                t => toks.push(t),
            };
        }

        assert_eq!(
            toks,
            vec![
                Token {
                    kind: Ident(Symbol::new(&['i', 'd', 'e', 'n', 't'])),
                    span: Span {
                        start_row: 0,
                        start_col: 0,
                        end_row: 0,
                        end_col: 0
                    }
                },
                Token {
                    kind: Eof,
                    span: Span {
                        start_row: 0,
                        start_col: 0,
                        end_row: 0,
                        end_col: 0
                    }
                }
            ]
        )

        //     assert_eq!(
        //         toks,
        //         vec![
        //             Token {
        //                 kind: OpenDel(Brace),
        //                 span: Span {
        //                     start_row: 0,
        //                     start_col: 0,
        //                     end_row: 0,
        //                     end_col: 0
        //                 }
        //             },
        //             Token {
        //                 kind: CloseDel(Brace),
        //                 span: Span {
        //                     start_row: 0,
        //                     start_col: 1,
        //                     end_row: 0,
        //                     end_col: 1
        //                 }
        //             },
        //             Token {
        //                 kind: FatArrow,
        //                 span: Span {
        //                     start_row: 1,
        //                     start_col: 0,
        //                     end_row: 1,
        //                     end_col: 1
        //                 }
        //             },
        //             Token {
        //                 kind: BinOpEq(Shl),
        //                 span: Span {
        //                     start_row: 1,
        //                     start_col: 2,
        //                     end_row: 1,
        //                     end_col: 2
        //                 }
        //             },
        //             Token {
        //                 kind: Eof,
        //                 span: Span {
        //                     start_row: 0,
        //                     start_col: 0,
        //                     end_row: 0,
        //                     end_col: 0
        //                 }
        //             }
        //         ]
        //     )
    }
}
