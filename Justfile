_default:
    just --list

# Run the CLI
run:
    cargo run --bin gdtoolkit

# Build all
build:
    cargo build

# Format all
fmt:
    cargo fmt --all

# Lint all
lint:
    cargo clippy --all --tests

# Test all
test:
    cargo test --all

# Test with coverage
test-cov:
    rm -rf .cov
    rm -rf htmlcov
    RUSTFLAGS="-Cinstrument-coverage" LLVM_PROFILE_FILE=".cov/test-%p-%m.profraw" cargo build
    RUSTFLAGS="-Cinstrument-coverage" LLVM_PROFILE_FILE=".cov/test-%p-%m.profraw" cargo test
    grcov . --binary-path ./target/debug/ -s . -t html --branch --ignore-not-existing --ignore "/*" -o htmlcov
