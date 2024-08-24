#[cfg(test)]
mod tests;

pub mod keyword;
pub mod token;

use keyword::Keyword;
use std::cell::Cell;
use std::fmt::Debug;
use token::*;

#[derive(Clone, PartialEq, Eq)]
pub struct Lexer {
    cur: Cell<usize>,
    bytes: Vec<u8>,
    lns: Vec<usize>,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pos = self.cur.get();
        write!(f, "Lexer {{ pos: {}, read_pos: {} }}", pos, pos + 1)
    }
}

impl Lexer {
    pub fn new<S: Into<Vec<u8>>>(src: S) -> Self {
        let bytes = src.into();
        Self {
            cur: Default::default(),
            lns: bytes
                .iter()
                .enumerate()
                .filter_map(|(br, &c)| (c == b'\n').then_some(br))
                .collect(),
            bytes,
        }
    }

    pub fn collect_tokens<'a, I>(&'a self) -> I
    where
        I: FromIterator<Token<'a>>,
    {
        FromIterator::from_iter(
            std::iter::from_fn(move || {
                let tok = self.next();
                (tok.kind != TokenKind::Eof).then_some(tok)
            })
            .chain(std::iter::once(self.build_eof())),
        )
    }

    pub(crate) fn next(&self) -> Token {
        self.consume_ws();
        let tok = match self.next_byte() {
            Some(&b) => match b {
                b'=' => self.read_eq(),
                b'!' => self.read_not(),
                b'.' => self.read_dot(),
                b'#' => self.read_hash(),
                b':' => self.read_colon(),
                b'-' => self.read_minus(),
                b'\'' | b'"' => self.read_str(),
                b'<' => self.read_angle(BinOperator::Shl, TokenKind::Lt, TokenKind::Le),
                b'>' => self.read_angle(BinOperator::Shr, TokenKind::Gt, TokenKind::Ge),
                b'|' => self.read_bw_or_bool(BinOperator::Or, TokenKind::OrOr),
                b'&' => self.read_bw_or_bool(BinOperator::And, TokenKind::AndAnd),
                b'^' => self.read_bin_op(BinOperator::Caret),
                b'+' => self.read_bin_op(BinOperator::Plus),
                b'%' => self.read_bin_op(BinOperator::Percent),
                b'*' => self.read_comp_bin_op(BinOperator::Star, BinOperator::StarStar),
                b'/' => self.read_comp_bin_op(BinOperator::Slash, BinOperator::SlashSlash),
                b'@' => self.build_tok(self.pos(), TokenKind::At),
                b',' => self.build_tok(self.pos(), TokenKind::Comma),
                b'~' => self.build_tok(self.pos(), TokenKind::Tilde),
                b';' => self.build_tok(self.pos(), TokenKind::Semicolon),
                b'{' => self.build_tok(self.pos(), TokenKind::OpenDel(Delimiter::Brace)),
                b'}' => self.build_tok(self.pos(), TokenKind::CloseDel(Delimiter::Brace)),
                b'[' => self.build_tok(self.pos(), TokenKind::OpenDel(Delimiter::Bracket)),
                b']' => self.build_tok(self.pos(), TokenKind::CloseDel(Delimiter::Bracket)),
                b'(' => self.build_tok(self.pos(), TokenKind::OpenDel(Delimiter::Parenthesis)),
                b')' => self.build_tok(self.pos(), TokenKind::CloseDel(Delimiter::Parenthesis)),
                r if r.is_ascii_digit() => return self.read_number(),
                r if r.is_ascii_alphabetic() || r == b'_' => return self.read_name(),
                _ => {
                    let pos = self.pos();
                    self.build_tok(
                        pos,
                        TokenKind::lit(LiteralKind::Error, Symbol::new(&self.bytes[pos..=pos])),
                    )
                }
            },
            None => return self.build_tok(self.pos(), TokenKind::Eof),
        };
        self.adv_pos();
        tok
    }

    fn build_tok<'a>(&'a self, start_pos: usize, kind: TokenKind<'a>) -> Token<'a> {
        if kind == TokenKind::Eof {
            return self.build_eof();
        }

        let end_pos = (start_pos < self.pos())
            .then(|| self.pos() - 1)
            .unwrap_or(start_pos);

        let start_line = self
            .lns
            .iter()
            .enumerate()
            .find_map(|(line, &br)| (start_pos < br).then_some(line))
            .unwrap_or(0);
        let end_line = self
            .lns
            .iter()
            .enumerate()
            .find_map(|(line, &br)| (end_pos < br).then_some(line))
            .unwrap_or(0);

        let (start_col, end_col) = if start_line > 0 && end_line >= start_line {
            (
                start_pos
                    .saturating_sub(self.lns[start_line - 1])
                    .saturating_sub(1),
                end_pos
                    .saturating_sub(self.lns[end_line - 1])
                    .saturating_sub(1),
            )
        } else {
            (start_pos, end_pos)
        };

        Token::new(
            kind,
            Span {
                start_line,
                start_col,
                end_line,
                end_col,
            },
        )
    }

    #[inline]
    fn build_eof(&self) -> Token {
        let line = self.lns.len().saturating_sub(1);
        let span = Span {
            start_line: line,
            start_col: 0,
            end_line: line,
            end_col: 0,
        };
        Token::new(TokenKind::Eof, span)
    }

    #[inline]
    fn next_byte(&self) -> Option<&u8> {
        self.bytes.get(self.cur.get())
    }

    #[inline]
    fn peek_next(&self) -> Option<&u8> {
        self.bytes.get(self.cur.get() + 1)
    }

    #[inline]
    fn peek_n(&self, n: usize) -> Option<&u8> {
        self.bytes.get(self.cur.get() + n)
    }

    #[inline]
    fn pos(&self) -> usize {
        self.cur.get()
    }

    #[inline(always)]
    fn adv_pos(&self) {
        _ = self.cur.replace(self.cur.get() + 1)
    }

    #[inline]
    fn consume_ws(&self) {
        while self.next_byte().is_some_and(|&b| b.is_ascii_whitespace()) {
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
        while self.next_byte().filter(|&&b| p(b)).is_some() {
            self.adv_pos();
        }
        r(start_pos, self.pos())
    }

    // +=============+
    // || SPECIFICS ||
    // +=============+

    fn read_not(&self) -> Token {
        if let Some(b'=') = self.peek_next() {
            return self.read_n_and(1, TokenKind::Ne);
        }
        self.build_tok(self.pos(), TokenKind::Not)
    }

    fn read_colon(&self) -> Token {
        if let Some(b':') = self.peek_next() {
            return self.read_n_and(1, TokenKind::ColonColon);
        }
        self.build_tok(self.pos(), TokenKind::Colon)
    }

    fn read_bin_op(&self, bin_op: BinOperator) -> Token {
        if let Some(b'=') = self.peek_next() {
            return self.read_n_and(1, TokenKind::BinOpEq(bin_op));
        }
        self.build_tok(self.pos(), TokenKind::BinOp(bin_op))
    }

    fn read_minus(&self) -> Token {
        match self.peek_next() {
            Some(b'>') => self.read_n_and(1, TokenKind::Arrow),
            Some(b'=') => self.read_n_and(1, TokenKind::BinOpEq(BinOperator::Minus)),
            _ => self.build_tok(self.pos(), TokenKind::BinOp(BinOperator::Minus)),
        }
    }

    fn read_dot(&self) -> Token {
        match (self.peek_next(), self.peek_n(2)) {
            (Some(b'.'), Some(b'.')) => self.read_n_and(2, TokenKind::Ellipsis),
            (Some(b'.'), _) => self.read_n_and(1, TokenKind::DotDot),
            _ => self.build_tok(self.pos(), TokenKind::Dot),
        }
    }

    fn read_eq(&self) -> Token {
        match self.peek_next() {
            Some(b'=') => self.read_n_and(1, TokenKind::EqEq),
            Some(b'>') => self.read_n_and(1, TokenKind::FatArrow),
            _ => self.build_tok(self.pos(), TokenKind::Eq),
        }
    }

    fn read_bw_or_bool<'a>(&'a self, bw: BinOperator, bool: TokenKind<'a>) -> Token<'a> {
        match self.peek_next() {
            Some(b) if b == self.next_byte().unwrap() => self.read_n_and(1, bool),
            Some(b'=') => self.read_n_and(1, TokenKind::BinOpEq(bw)),
            _ => self.build_tok(self.pos(), TokenKind::BinOp(bw)),
        }
    }

    fn read_comp_bin_op(&self, sing_op: BinOperator, comp_op: BinOperator) -> Token {
        match self.peek_next() {
            Some(n) if n == self.next_byte().unwrap() => match self.peek_n(2) {
                Some(b'=') => self.read_n_and(2, TokenKind::BinOpEq(comp_op)),
                _ => self.read_n_and(1, TokenKind::BinOp(comp_op)),
            },
            Some(b'=') => self.read_n_and(1, TokenKind::BinOpEq(sing_op)),
            _ => self.build_tok(self.pos(), TokenKind::BinOp(sing_op)),
        }
    }

    fn read_angle<'a>(
        &'a self,
        bw: BinOperator,
        bool: TokenKind<'a>,
        bool_eq: TokenKind<'a>,
    ) -> Token<'a> {
        match self.peek_next() {
            Some(n) if n == self.next_byte().unwrap() => match self.peek_n(2) {
                Some(b'=') => self.read_n_and(2, TokenKind::BinOpEq(bw)),
                _ => self.build_tok(self.pos(), TokenKind::BinOp(bw)),
            },
            Some(b'=') => self.read_n_and(1, bool_eq),
            _ => self.build_tok(self.pos(), bool),
        }
    }

    fn read_hash(&self) -> Token {
        let org_pos = self.pos();
        self.adv_pos();

        if self.next_byte().filter(|&&b| b != b'*').is_some() {
            return self.read_while_and(
                |b| b != b'\n',
                |s, l| {
                    self.build_tok(
                        org_pos,
                        TokenKind::Comment(CommentKind::Inline, Symbol::new(&self.bytes[s..l])),
                    )
                },
            );
        }

        self.adv_pos();
        self.read_while_and(
            |b| b != b'*',
            |s, l| match self.peek_next() {
                Some(b'#') => {
                    self.adv_pos();
                    self.adv_pos();
                    self.build_tok(
                        org_pos,
                        TokenKind::Comment(CommentKind::Block, Symbol::new(&self.bytes[s..l])),
                    )
                }
                _ => self.build_tok(
                    org_pos,
                    TokenKind::lit(
                        LiteralKind::Error,
                        Symbol::new(&self.bytes[org_pos..=l - 1]),
                    ),
                ),
            },
        )
    }

    fn read_name(&self) -> Token {
        self.read_while_and(
            |b| b.is_ascii_alphanumeric() || b == b'_',
            |s, l| {
                let kind = Keyword::try_from_bytes(&self.bytes[s..l])
                    .map(TokenKind::Keyword)
                    .unwrap_or_else(|| TokenKind::Ident(Symbol::new(&self.bytes[s..l])));
                self.build_tok(s, kind)
            },
        )
    }

    fn read_str(&self) -> Token {
        let f = *self.next_byte().unwrap();
        let org_pos = self.pos();
        self.adv_pos();

        self.read_while_and(
            |b| b != b'"' && b != b'\'',
            |s, l| {
                if self.next_byte().filter(|&&c| c == f).is_some() {
                    return self.build_tok(
                        org_pos,
                        TokenKind::lit(LiteralKind::Str, Symbol::new(&self.bytes[s..l])),
                    );
                }
                self.build_tok(
                    org_pos,
                    TokenKind::lit(LiteralKind::Error, Symbol::new(&self.bytes[org_pos..=l])),
                )
            },
        )
    } // TODO: read scaped strings

    fn read_number(&self) -> Token {
        self.read_while_and(
            |b| b.is_ascii_digit() || b == b'.',
            |s, l| {
                let bytes = &self.bytes[s..l];
                let kind = bytes.contains(&b'.')
                    .then_some(LiteralKind::Float)
                    .unwrap_or(LiteralKind::Int);
                self.build_tok(s, TokenKind::lit(kind, Symbol::new(bytes)))
            },
        )
    } // TODO: look for multiple periods in the float numbers
}
