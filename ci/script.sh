# This script takes care of testing your crate

set -ex

cargo build --target $TARGET
cargo build --target $TARGET --release
cargo test --target $TARGET
cargo test --target $TARGET --release
cargo run --target $TARGET
cargo run --target $TARGET --release
