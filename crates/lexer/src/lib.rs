#![allow(non_upper_case_globals)]
#![feature(let_chains)]

use std::cell::Ref;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::RangeBounds;
use std::slice::SliceIndex;

pub mod keyword;
pub mod token;

use token::BinOpToken::{self, *};
use token::CommentKind::*;
use token::DelimiterToken::*;
use token::LitKind::*;
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
        }
    }

    pub fn next_tok(&self) -> Token<'_> {
        let tok = match self.char() {
            Some(&ch) => match ch {
                // TODO: Handle ', " and errors
                '@' => Token::new(At),
                ',' => Token::new(Comma),
                '~' => Token::new(Tilde),
                ';' => Token::new(Semicolon),
                '{' => Token::new(OpenDel(Brace)),
                '}' => Token::new(CloseDel(Brace)),
                '[' => Token::new(OpenDel(Bracket)),
                ']' => Token::new(CloseDel(Bracket)),
                '(' => Token::new(OpenDel(Parenthesis)),
                ')' => Token::new(CloseDel(Parenthesis)),
                '#' => todo!(),
                '=' => self.eq_or_comp(),
                '.' => self.dot_or_comp(),
                '!' => self.not_or_comp(),
                ':' => self.colon_or_comp(),
                '-' => self.minus_or_comp(),
                '/' => self.slash_or_comp(),

                '<' => self.bin_or_comp(Shl),
                '>' => self.bin_or_comp(Shr),
                '*' => self.bin_or_comp(Star),
                '+' => self.bin_or_comp(Plus),
                '^' => self.bin_or_comp(Caret),
                '%' => self.bin_or_comp(Percent),
                '|' => self.bw_or_comp(Or, OrOr),
                '&' => self.bw_or_comp(And, AndAnd),

                _ => todo!(),
            },
            None => Token::new(Eof),
        };
        self.adv_pos();
        tok
    }

    #[inline]
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

    fn adv_pos(&self) {
        let mut state = self.state.borrow_mut();
        if self.chars.get(state.pos).is_some() {
            state.pos += 1;
            state.read_pos += 1;
        }
    }

    fn read_chars(&self) -> &[char] {
        let state = self.state.borrow();
        let curr_pos = state.pos;
        while self.is_letter() {
            self.adv_pos()
        }
        &self.chars[curr_pos..state.pos]
    }

    fn is_letter(&self) -> bool {
        self.char().is_some_and(|&c| c.is_alphabetic() || c == '_')
    }

    fn is_numeric(&self) -> bool {
        self.char().is_some_and(|&c| c.is_numeric())
    }

    fn sing_or_comp<'a, F>(&'a self, f: F) -> Token<'a>
    where
        F: FnOnce(Ref<'a, State>, Option<&'a char>) -> Token<'a>,
    {
        f(self.state.borrow(), self.peek_next())
    }

    fn read_while_and<'a, P, F>(&'a self, p: P, f: F) -> Token<'a>
    where
        P: Fn() -> bool,
        F: FnOnce(usize, usize) -> Token<'a>,
    {
        let state = self.state.borrow();
        let curr_pos = state.pos;
        while p() {
            self.adv_pos();
        }
        f(curr_pos, state.pos)
    }

    fn read_n<'a>(&'a self, n: usize, kind: TokenKind<'a>) -> Token<'a> {
        (0..n).for_each(|_| self.adv_pos());
        Token::new(kind)
    }

    #[inline]
    fn minus_or_comp(&self) -> Token<'_> {
        self.sing_or_comp(|_, n| match n {
            Some('>') => self.read_n(1, Arrow),
            Some('=') => self.read_n(1, BinOpEq(Minus)),
            _ => Token::new(BinOp(Minus)),
        })
    }

    #[inline]
    fn not_or_comp(&self) -> Token<'_> {
        self.sing_or_comp(|_, n| match n {
            Some('=') => self.read_n(1, Ne),
            _ => Token::new(Not),
        })
    }

    #[inline]
    fn dot_or_comp(&self) -> Token<'_> {
        self.sing_or_comp(|_, n| match (n, self.peek_n(2)) {
            (Some('.'), Some('.')) => self.read_n(2, Ellipsis),
            (Some('.'), _) => self.read_n(1, DotDot),
            _ => Token::new(Dot),
        })
    }

    #[inline]
    fn eq_or_comp(&self) -> Token<'_> {
        self.sing_or_comp(|_, n| match n {
            Some('=') => self.read_n(1, EqEq),
            Some('>') => self.read_n(1, FatArrow),
            _ => Token::new(Eq),
        })
    }

    #[inline]
    fn colon_or_comp(&self) -> Token<'_> {
        self.sing_or_comp(|_, n| match n {
            Some(':') => self.read_n(1, ColonColon),
            _ => Token::new(Colon),
        })
    }

    #[inline]
    fn bw_or_comp<'a>(&'a self, bw: BinOpToken, comp: TokenKind<'a>) -> Token<'a> {
        self.sing_or_comp(|_, n| match n {
            Some(c) if c == self.char().unwrap() => self.read_n(1, comp),
            _ => Token::new(BinOp(bw)),
        })
    }

    fn bin_or_comp<'a>(&'a self, bin_op: BinOpToken) -> Token<'a> {
        // TODO: FIX - Change comment syms
        self.sing_or_comp(|_, n| match (n, self.peek_n(2)) {
            (Some('='), _) => self.read_n(1, BinOpEq(bin_op)),
            (Some(a), t) => match (a, t) {
                ('<' | '>' | '*' | '/', Some('=')) => self.read_n(2, BinOpEq(bin_op)),
                _ => self.read_n(1, BinOp(bin_op)),
            },
            _ => Token::new(BinOp(bin_op)),
        })
    }

    fn slash_or_comp(&self) -> Token<'_> {
        self.sing_or_comp(|_, n| match (n, self.peek_n(2)) {
            (Some('/'), Some('=')) => self.read_n(2, BinOpEq(SlashSlash)),
            (Some('/'), Some('*')) => self.read_while_and(
                || *self.char().unwrap() != '*',
                |c, l| match self.peek_n(l) {
                    Some('/') => Token::new(Comment(Block, Symbol::new(&self.chars[c..=l]))),
                    _ => Token::new(TokenKind::lit(Error, Symbol::new(&self.chars[c..=l]))),
                },
            ),
            (Some('/'), _) => self.read_while_and(
                || *self.char().unwrap() != '\n',
                |c, l| Token::new(Comment(Inline, Symbol::new(&self.chars[c..=l]))),
            ),
            _ => Token::new(BinOp(Slash)),
        })
    }

    fn _lit_err<R>(&self, range: R) -> Token<'_>
    where
        R: RangeBounds<usize>,
        R: SliceIndex<[char], Output = [char]>,
    {
        Token::new(TokenKind::lit(Error, Symbol::new(&self.chars[range])))
    }
}
