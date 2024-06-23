#!/bin/bash
unzip -o $1 -d example_code/
dune exec scratch_compiler
gcc out.o -L . -lruntime
./a.out
