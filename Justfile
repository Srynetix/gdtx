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
