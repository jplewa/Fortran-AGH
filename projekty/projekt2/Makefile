FLAGS = -O2 -std08 -module . -free -implicitnone -cpp -warn all -funroll-loops
.PHONY: test plots

test: 
	ifort -c ./src/mult.F90 $(FLAGS)
	make -C test test
	make -C test
	make clean
	./bin/test

plots:  
	ifort -c ./src/mult.F90 $(FLAGS)
	ifort -o ./bin/mult_time ./src/mult_time.F90 mult.o $(FLAGS)
	make clean
	./bin/mult_time
	julia ./src/julia/plots.jl

clean:
	rm -f *.o *.mod *.i90 *~ *.tmp *.s *.dbg
	rm -f -r *.dSYM
