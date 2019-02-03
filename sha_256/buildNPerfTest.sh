#!/bin/bash
set -e
rm *.mod ||:
#gfortran -std=f2008 -O3 -Wall -march=native mod_sha256.f90 perf_sha256.f90
gfortran -std=f2008 -O3 -Wall -march=native -mavx -mavx2 mod_sha256.f90 perf_sha256.f90
./a.out
