use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer;

fn bench_lexer_tokenization(c: &mut Criterion) {
    let src = include_str!("../testdata/snapshots/code_snipet.gl");
    let lexer = lexer::Lexer::new(src);

    c.bench_function("lexer: code_snipet.gl tokenization", |b| {
        let mut toks: Vec<lexer::token::Token> = Vec::new();

        b.iter(black_box(#[inline(always)] || {
            loop {
                let tok = lexer.next_tok();
                if tok.kind == lexer::token::TokenKind::Eof {
                    break toks.push(tok);
                }
                toks.push(tok);
            }
        }))
    });
}

criterion_group!(lexer_benches, bench_lexer_tokenization);
criterion_main!(lexer_benches);
