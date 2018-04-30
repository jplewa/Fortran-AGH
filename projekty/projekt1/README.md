# Heat equation FDM solver and average error calculator
Project for a university Fortran class.  
Generates output files with the average numerical error of heat equation solutions for grids in range [1, N] with step M for all three types of real number precision available in Fortran. Additionally, a JuliaLang program generates plots based on the results.

### Prerequisites
Tested with ifort 18.0.1 20171018.  
Plots generated with Julia 0.6.2 using packages: Plots, PlotlyJS, RSVG, and Formatting.
To add those packages, run
```
Pkg.add("Plots")
Pkg.add("PlotlyJS")
Pkg.add("RSVG")
Pkg.add("Formatting")
Pkg.update()
```
in your Julia console.

### Testing
Tests of the Gauss-Jordan matrix elimination algorithm can be run with:
```
make tests
```

### Building
To build the project, type:
```
make build
```

### Running
After building, run the program with
```
./bin/project N M
```
to generate files with errors for grids in range [1,N] each time incrementing by M.
Output files for the respective real number kinds can be found in:
```
./out/results/
```
For plotting purposes, an additional file with grid sizes is generated as well.

### Plotting
To generate plots with the results from 
```
./out/results/
```
type:
```
make plots
```
The plots will be saved in the following location:
```
./out/plots/
```
in the PDF format.

### Entire project
Alternatively, to complete all of the steps mentioned above - run the tests, build the project, run the program specifically for N in range [1,1000], and then plot the results - you can type
```
make all
```