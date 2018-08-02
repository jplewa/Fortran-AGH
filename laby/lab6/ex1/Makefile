FLAGS = -O2 -std08 -module . -free -implicitnone -cpp -warn all -funroll-loops

test: 
	ifort -c ./mat_ops.F90 $(FLAGS)
	make clean

f2py:
	make test
	F77=ifort F90=ifort CC=icc f2py -c mat_ops.F90 -m mat_ops

clean:
	rm -f *.o *.mod *.i90 *~ *.tmp *.s *.dbg
	rm -f -r *.dSYM