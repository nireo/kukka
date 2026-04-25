.PHONY: fmt test test-examples bench bench-build check

fmt:
	cargo fmt

test:
	cargo test

test-examples:
	cargo test --examples

bench:
	cargo bench --bench parsers

bench-build:
	cargo bench --bench parsers --no-run

check: fmt test test-examples bench-build
