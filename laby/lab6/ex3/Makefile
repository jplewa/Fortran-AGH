FLAGS = -O2 -std08 -module . -free -implicitnone -cpp -warn all

test: 
	ifort -c ../ex2/co_mat_ops.F90 -coarray $(FLAGS)
	ifort -o program main.F90 co_mat_ops.o -coarray $(FLAGS)
	make clean

clean:
	#rm -f *.o *.mod *.i90 *~ *.tmp *.s *.dbg
	#rm -f -r *.dSYM