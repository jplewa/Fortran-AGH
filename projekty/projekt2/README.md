# Matrix multiplication
Project for a university Fortran class.  
Generates output files with time measurements of five different matrix multiplication methods and plots them.

### Prerequisites
Tested with ifort 18.0.1 20171018.  
Plots generated with Julia 0.6.2 using packages: DataFrames and Gadfly.
To add those packages, run
```
Pkg.add("Gadfly")
Pkg.add("DataFrames")
Pkg.update()
```
in your Julia console.

### Testing
Tests of the five matrix multiplication algorithms can be built and run with:
```
make test
```

### Running and plotting
To run the program and generate a plot of the results, type
```
make plots
```

The plot will be saved in the following location:
```
./out/plots/
```
in the PNG format.