.PHONY: test plots

test: 
	make -C test test
	make -C test
	./bin/test

plots:  
	ifort -c ./src/mult.F90 -std08 -O2 -warn all -pedantic -funroll-loops
	ifort -o ./bin/mult_time ./src/mult_time.F90 mult.o -std08 -O2 -warn all -pedantic -funroll-loops
	make clean
	./bin/mult_time
	julia ./src/julia/plots.jl
clean:
	rm -f *.o *.mod *.i90 *~ *.tmp *.s *.dbg
	rm -f -r *.dSYM
