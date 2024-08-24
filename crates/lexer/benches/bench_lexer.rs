use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer;

fn bench_lexer_collect_tokens(c: &mut Criterion) {
    let lexer = lexer::Lexer::new(
        include_str!("../testdata/snapshots/code_snipet.gl"));
    c.bench_function("lexer: code_snipet.gl tokenization", |b| {
        b.iter(black_box(|| _ = lexer.collect_tokens::<Vec<_>>()))
    });
}

criterion_group!(lexer_benches, bench_lexer_collect_tokens);
criterion_main!(lexer_benches);
