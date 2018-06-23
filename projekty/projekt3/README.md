# Matrix operations - coarrays

Project for a university Fortran class.  

Performs matrix operations sequentially and in a parallel way.

The three excercises are:

* `ex1` - sequential matrix multiplication and Gauss elimination and connecting Fortran with Python
* `ex2` - parallel matrix multiplication
* `ex3` - time measurements of both methods

## Prerequisites

Tested with:

* `IFORT 18.0.1`
* `Python 3.6.4`
* `f2py 2`
* `doxygen 1.8.15`

## Documentation

To generate HTML documentation, type

```
make docs
```

## Python integration

To test the f2py functionality, type

```
make python_test
```

## Matrix multiplication speed comparison

To see the speed comparison of parallel and sequential matrix multiplication, run

```
make test
```

