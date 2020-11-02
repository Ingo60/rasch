#!/bin/bash
cargo clean
rm -rf /tmp/merged.profdata /tmp/pgo-data
RUSTFLAGS="-Cprofile-generate=/tmp/pgo-data" cargo build --release
echo generating profile data with instrumented binary, this will take a while ...
./target/release/rasch flamegraph 8
PATH=/home/ingo/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/bin/:$PATH
llvm-profdata merge -o /tmp/merged.profdata /tmp/pgo-data
cargo clean
RUSTFLAGS="-Cprofile-use=/tmp/merged.profdata" cargo build --release
echo check the success, this will take a shorter while
echo should be better than
echo ' 0 25 16289 94563529 b1c3 d7d5 d2d4 e7e6 c1f4 f8b4 a2a3 b4c3 b2c3'
./target/release/rasch flamegraph 8
