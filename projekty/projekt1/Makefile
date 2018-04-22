IFORT = ifort
OPTIONS = -std08 -module . -free -implicitnone -cpp -warn all -check bounds -fast

all:
	 $(IFORT) -c matrix_operations.f90 -c $(OPTIONS)
	 $(IFORT) -c heat_equation.f90 $(OPTIONS)
	 $(IFORT) -o i_tests tests.f90 matrix_operations.o $(OPTIONS)
	 $(IFORT) -o i_projekt projekt.f90 heat_equation.o matrix_operations.o $(OPTIONS)

	make clean
	make tests
	make project

clean:
	rm -f *.o
	rm -f *.mod

project:
	 ./i_projekt

tests:
	./i_tests