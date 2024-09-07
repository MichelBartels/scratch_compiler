#!/bin/bash
unzip -o $1 -d example_code/
dune exec scratch_compiler
cargo build --release --manifest-path ../scratch-compiler-runtime/Cargo.toml
cp ../scratch-compiler-runtime/target/release/libruntime.a .
gcc out.o -L . -lruntime -lgcc_s -lutil -lrt -lpthread -lm -ldl -lc
./a.out
