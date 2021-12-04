use chess::board;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("for_bitboard_ones");
    for i in [0x0102040801020408, 0x80].iter() {
        group.bench_with_input(BenchmarkId::new("iterative", i), i, |b, i| {
            b.iter(|| {
                let mut count = 0;
                board::for_bitboard_ones(black_box(*i), |rank, file| {
                    count += 1;
                });
            })
        });
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
