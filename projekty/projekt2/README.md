# Matrix multiplication
Project for a university Fortran class.  

Generates output files with time measurements of five different matrix multiplication methods and plots them.

The methods are:
* `mm1` - basic three-loop matrix multiplication
* `mm2` - matrix multiplication making use of the built-in Fortran dot product function 
* `mm3` - matrix multiplication with optimized cache access
* `mm4` - matrix multiplication with optimized cache access making use of the built-in dot product function
* `matmul` - built-in matrix multiplication Fortran function

### Prerequisites
Tested with `IFORT 18.0.1`. 

Plots generated with `Julia 0.6.2` using packages: `DataFrames` and `Gadfly`.

To add those packages, run
```
Pkg.add("Gadfly")
Pkg.add("DataFrames")
Pkg.update()
```
in your Julia console.

Unit tests run with `pFUnit 3.2.9`.

### Testing
Tests of the five matrix multiplication algorithms can be built and run with:
```
make test
```

### Running and plotting
To run the program and generate a plot of time measurements of the five different matrix multiplication algorithms for matrices in range [1,1001] , type
```
make plots
```

The plot will be saved in the following location:
```
./out/plots/
```
in the PNG format.