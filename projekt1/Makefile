IFORT = ifort
OPTIONS = -std08 -module . -free -implicitnone -cpp -warn all -check bounds -O2

all:
	 $(IFORT) projekt.f90 -o i_projekt $(OPTIONS)
	make clean
	make test

clean:
	rm -f *.o

test:
	 ./i_projekt
