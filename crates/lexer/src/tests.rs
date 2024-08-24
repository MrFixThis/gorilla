use crate::TokenKind::Eof;
use std::collections::VecDeque;

use super::*;

macro_rules! snapshot {
    ( $( ($func:ident, $path:expr) ),* $(,)? ) => {
        $(
            #[test]
            fn $func() {
                let src = include_str!($path);
                let mut conf = ::insta::Settings::clone_current();
                conf.set_snapshot_path("../testdata/output/");
                conf.bind(|| ::insta::assert_snapshot!(prepare_snapshot(src)));
            }
        )*
    };
}

fn prepare_snapshot(src: &str) -> String {
    let lexer = Lexer::new(src);
    let mut toks: VecDeque<_> = lexer.collect_tokens();
    let mut out = String::new();

    for (line, row) in src.lines().enumerate() {
        out.push_str(row);
        out.push('\n');

        while let Some(tok) = toks.pop_front() {
            if tok.span.start_line != line && tok.kind != Eof {
                toks.push_front(tok);
                break;
            }

            out.push_str(&" ".repeat(tok.span.start_col));
            out.push_str(&"^".repeat((tok.span.end_col - tok.span.start_col) + 1));
            out.push_str(&format!(" {tok:?}\n"));
        }
    }

    out
}

snapshot!(
    (
        test_code_snipet_tokenization,
        "../testdata/snapshots/code_snipet.gl"
    ),
    (
        test_comment_tokenization,
        "../testdata/snapshots/comment.gl"
    ),
    (
        test_container_types_tokenization,
        "../testdata/snapshots/container_type.gl"
    ),
    (
        test_literal_error_tokenization,
        "../testdata/snapshots/literal_error.gl"
    ),
    (
        test_function_tokenization,
        "../testdata/snapshots/function.gl"
    ),
    (
        test_identifier_tokenization,
        "../testdata/snapshots/identifier.gl"
    ),
    (
        test_literal_tokenization,
        "../testdata/snapshots/literal.gl"
    ),
);
