IFORT = ifort
OPTIONS = -std08 -module . -free -implicitnone -cpp -warn all -check bounds -fast

all: 
	make build
	make test
	make project
	make plots

build:
	 $(IFORT) -c src/matrix_operations.f90 -c $(OPTIONS)
	 $(IFORT) -c src/heat_equation.f90 $(OPTIONS)
	 $(IFORT) -o bin/tests tests/tests.f90 matrix_operations.o $(OPTIONS)
	 $(IFORT) -o bin/project src/project.f90 heat_equation.o matrix_operations.o $(OPTIONS)
	 make clean

clean:
	rm -f *.o
	rm -f *.mod

test:
	./bin/tests

project:
	 ./bin/project 1000 1

plots: 
	 julia src/plots.jl
