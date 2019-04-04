#!/bin/bash
set -e
rm *.mod ||:
gfortran -std=f2008 -O3 -Wall mod_sha256.f90 cmd_sha256.f90 -o sha256cmd
