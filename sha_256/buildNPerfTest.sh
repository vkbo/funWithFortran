#!/bin/bash
set -e
rm *.mod ||:
gfortran -O3 mod_sha256.f90 perf_sha256.f90
./a.out
# xxd test.bin
