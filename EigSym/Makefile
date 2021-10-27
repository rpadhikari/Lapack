FC=gfortran -c
LD=gfortran
SRC=dsyev.f90
OBJ=dsyev.o
dsyev:
	$(FC) $(SRC)
	$(LD) $(OBJ) -o eiv.x -llapack -lblas
	rm *.o
clean:
	rm -rf *.o *.x

