use std::collections::HashMap;

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use kukka::*;
use rustc_hash::FxHashMap;

fn comma_separated_numbers(count: usize) -> String {
    (0..count)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join(",")
}

fn key_value_pairs(count: usize) -> String {
    (0..count)
        .map(|n| format!("k{n}={n}"))
        .collect::<Vec<_>>()
        .join(",")
}

fn bench_primitives(c: &mut Criterion) {
    let mut group = c.benchmark_group("primitives");

    group.bench_function("integer", |b| {
        b.iter(|| black_box(integer.parse(black_box("1234567890 rest"))))
    });

    group.bench_function("double", |b| {
        b.iter(|| black_box(double.parse(black_box("12345.67890 rest"))))
    });

    group.bench_function("take_while", |b| {
        let parser = take_while(|c: char| c.is_ascii_alphabetic());
        b.iter(|| black_box(parser.parse(black_box("abcdefghijklmnopqrstuvwxyz123"))))
    });

    group.bench_function("take_while1", |b| {
        let parser = take_while1(|c: char| c.is_ascii_alphabetic());
        b.iter(|| black_box(parser.parse(black_box("abcdefghijklmnopqrstuvwxyz123"))))
    });

    group.bench_function("multispace0", |b| {
        b.iter(|| black_box(multispace0.parse(black_box("          abc"))))
    });

    group.finish();
}

fn bench_separated_integers(c: &mut Criterion) {
    let mut group = c.benchmark_group("separated integers");

    for count in [10, 1_000, 10_000] {
        let input = comma_separated_numbers(count);
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("separated1", count), &input, |b, input| {
            let parser = separated1(integer, char(','));
            b.iter(|| black_box(parser.parse(black_box(input.as_str()))))
        });
    }

    group.finish();
}

fn bench_separated_fold(c: &mut Criterion) {
    let mut group = c.benchmark_group("separated fold");

    for count in [10, 1_000, 10_000] {
        let input = key_value_pairs(count);
        group.throughput(Throughput::Elements(count as u64));

        group.bench_with_input(BenchmarkId::new("HashMap", count), &input, |b, input| {
            let parser = separated_fold(
                separated_pair(take_while1(|c| c != '='), char('='), integer),
                char(','),
                HashMap::new,
                |mut map, (key, value)| {
                    map.insert(key, value);
                    map
                },
            );

            b.iter(|| black_box(parser.parse(black_box(input.as_str()))))
        });

        group.bench_with_input(BenchmarkId::new("FxHashMap", count), &input, |b, input| {
            let parser = separated_fold(
                separated_pair(take_while1(|c| c != '='), char('='), integer),
                char(','),
                FxHashMap::default,
                |mut map, (key, value)| {
                    map.insert(key, value);
                    map
                },
            );

            b.iter(|| black_box(parser.parse(black_box(input.as_str()))))
        });
    }

    group.finish();
}

fn bench_chaining(c: &mut Criterion) {
    let input = comma_separated_numbers(1_000);
    let mut group = c.benchmark_group("chaining");
    group.throughput(Throughput::Elements(1_000));

    group.bench_function("free functions", |b| {
        let parser = separated1(map(integer, |n| n * 2), char(','));
        b.iter(|| black_box(parser.parse(black_box(input.as_str()))))
    });

    group.bench_function("method chaining", |b| {
        let parser = integer.map(|n| n * 2).separated1(char(','));
        b.iter(|| black_box(parser.parse(black_box(input.as_str()))))
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_primitives,
    bench_separated_integers,
    bench_separated_fold,
    bench_chaining,
);
criterion_main!(benches);
