FLAGS = -O2 -std08 -module . -free -implicitnone -cpp -warn all -funroll-loops

.PHONY: docs

python_test:
	make -C ex1 python_test
	python3 ./ex1/python_test.py
	make clean

docs:
	doxygen doxconfig
	make clean

test:
	ifort -c ./ex2/parallel_mat_ops.F90 -o ./ex3/parallel_mat_ops.o -coarray $(FLAGS)
	ifort -c ./ex1/sequential_mat_ops.F90 -o ./ex3/sequential_mat_ops.o -coarray $(FLAGS)
	ifort -o ./ex3/program ./ex3/main.F90 ./ex3/parallel_mat_ops.o ./ex3/sequential_mat_ops.o -I../ex2 -I../ex1 -coarray -heap-arrays $(FLAGS)
	make clean
	./ex3/program

clean:
	rm -f *.o *.mod *.i90 *~ *.tmp *.s *.dbg
	rm -f -r *.dSYM