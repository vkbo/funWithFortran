# Fun with Fortran!

Sometimes I look at algorithms and code and wonder if it would be easy to implement this in Fortran.

In this repository I will add some of the Fortran modules I come up with.

All the code is written following the Fortran 2008 standard, although I use the `.f90` file name extension.
And tested using the GNU gfortran compiler.

## Content

### Strings

The `strings` module is an implementation of a string object inspired by the one found in C++
It has some operator overloading, and can be combined with standard Fortran character arrays.

The implementation is similar to ISO_VARYING_STRING that was a proposed extension to Fortran 90, which had known flaws.
It has worked well for what I've used it for, but it doesn't really give many benefits over plain allocatable character arrays.

### SHA-256

The SHA-256 module implements the 256 bit SHA-2 digest algorithm in Fortran 2008.
It probably only works on little endian machines.

The bash script `buildCmd.sh` will build a command line executable `sha256cmd` that will compute the hash of a string taken as an argument.
The bash script `buildNTest.sh` will build a small test executable and run 4 test hashes.
The bash script `buildNPerfTest.sh` will build a small test executable and run the lorem ipsum digest 1 million times.
This takes about 3 seconds on my Intel i7-7700K CPU.

This module is tested with gfrortran 8.2, ifort 18.0.3 and nagfor 6.1.
There are no issues with gfortran, it does not work with ifort, and works with a few warnings on nagfor.
