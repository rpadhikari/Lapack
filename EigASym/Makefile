FC=gfortran -c
LD=gfortran
SRC=dgeev.f90
OBJ=dgeev.o
dsyev:
	$(FC) $(SRC)
	$(LD) $(OBJ) -o eiv.x -llapack -lblas
	rm *.o
clean:
	rm -rf *.o *.x

