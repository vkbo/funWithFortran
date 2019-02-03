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

