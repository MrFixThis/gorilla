use std::collections::VecDeque;

use super::*;

macro_rules! snapshot {
    ( $func:ident, $path:expr ) => {
        #[test]
        fn $func() {
            let src = include_str!($path);
            let mut conf = ::insta::Settings::clone_current();
            conf.set_snapshot_path("../testdata/output/");
            conf.bind(|| ::insta::assert_snapshot!(prepare_snapshot(src)));
        }
    };
}

fn prepare_snapshot(src: &str) -> String {
    let lexer = Lexer::new(src);
    let mut toks: VecDeque<Token> = VecDeque::new();

    loop {
        let tok = lexer.next_tok();
        if tok.kind == Eof {
            break toks.push_back(tok);
        }
        toks.push_back(tok);
    }

    let mut out = String::new();
    for (row, line) in src.lines().enumerate() {
        out += line;
        out += "\n";

        while let Some(tok) = toks.pop_front() {
            if tok.span.start_row != row && tok.kind != Eof {
                toks.push_front(tok);
                break;
            }

            out += &" ".repeat(tok.span.start_col);
            out += &"^".repeat((tok.span.end_col - tok.span.start_col) + 1);
            out += &format!(" {tok:?}\n");
        }
    }

    out
}

snapshot!(test_code_snipet_tokenization, "../testdata/snapshots/code_snipet.gl");
snapshot!(test_comment_tokenization, "../testdata/snapshots/comment.gl");
snapshot!(test_container_types_tokenization, "../testdata/snapshots/container_type.gl");
snapshot!(test_literal_error_tokenization, "../testdata/snapshots/literal_error.gl");
snapshot!(test_function_tokenization, "../testdata/snapshots/function.gl");
snapshot!(test_identifier_tokenization, "../testdata/snapshots/identifier.gl");
snapshot!(test_literal_tokenization, "../testdata/snapshots/literal.gl");
