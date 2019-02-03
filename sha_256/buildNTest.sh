#!/bin/bash
set -e
rm *.mod ||:
gfortran mod_sha256.f90 test_sha256.f90
./a.out
# xxd test.bin
