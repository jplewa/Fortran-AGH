IFORT = ifort
OPTIONS = -std08 -module . -free -implicitnone -cpp -warn all -check bounds -O2

all:
	 $(IFORT) -c gauss_elim.f90 -c $(OPTIONS)
	 $(IFORT) -c tests.f90 $(OPTIONS)
	 $(IFORT) -c heat_equation.f90 $(OPTIONS)
	 $(IFORT) -o i_projekt projekt.f90 heat_equation.o  gauss_elim.o $(OPTIONS)

	make clean
	make test

clean:
	rm -f *.o

test:
	 ./i_projekt
