#!/bin/bash
set -e
rm *.mod ||:
gfortran -std=f2008 -O3 -Wall mod_sha256.f90 test_sha256.f90
#ifort -std08 -O3 mod_sha256.f90 test_sha256.f90
#nagfor -O4 mod_sha256.f90 test_sha256.f90
./a.out
