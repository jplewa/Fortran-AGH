# Heat equation FDM solver and average error calculator
Project for a university Fortran class.  
Generates output files with the average numerical error of heat equation solutions for grids in range [1, N] with step M (where N and M are program arguments) for all three types of real number precision available in Fortran. Additionally, a JuliaLang program generates plots of the errors.

### Prerequisites
Tested with ifort version 18.0.1 20171018.  
Plots generated with Julia version 0.6.2 using packages: Plots, PlotlyJS, RSVG.  
To add those packages run
```
Pkg.add("Plots")
Pkg.add("PlotlyJS")
Pkg.add("RSVG")
Pkg.update()
```
in your Julia console.

### Testing
Tests of Gauss-Jordan matrix elimination can be run with
```
make tests
```

### Building
To build the project type
```
make build
```

### Running
After building, the project can be run with
```
./bin/projekt
```
and then provide N and M to generate files with average numerical error of heat equation solutions for grids in range [1,N] with step M. Output files for all real number kinds can be found in
```
./out/results/
```
For plotting purposes, a file with all the grid sizes can be found there as well.

### Plotting
To generate plots with the results from 
```
./out/results/
```
type
```
make plots
```
The plots will be saved in 
```
./out/plots/
```
in the PDF format.