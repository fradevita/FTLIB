FC=gfortran
FLAGS=-Wall -g -fcheck=all -cpp -DVERBOSE --fpe-trap=invalid,zero,overflow
##FLAGS=-Ofast -flto -cpp

LIBOBJS= ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_root.o ftlib_quadrature.o \
ftlib_quadrature_rectangle.o ftlib_quadrature_trapezoidal.o ftlib_quadrature_gauss.o  ftlib_grid.o \
ftlib_scalar.o ftlib_polynomials.o ftlib_fields.o ftlib_utils.o

lib: $(LIBOBJS)
	ar r libftlib.a $(LIBOBJS); mv libftlib.a ../lib; rm *.o; mv *.mod ../include; mv *.smod ../include

ftlib_utils.o: ftlib_utils.f90 ftlib_kinds.o
	$(FC) $(FLAGS) -c $<

ftlib_polynomials.o: ftlib_polynomials.f90 ftlib_kinds.o ftlib_grid.o ftlib_function.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_fields.o: ftlib_fields.f90 ftlib_scalar.o
	$(FC) $(FLAGS) -c $<

ftlib_scalar.o: ftlib_scalar.f90 ftlib_kinds.o ftlib_grid.o
	$(FC) $(FLAGS) -c $<

ftlib_grid.o: ftlib_grid.f90 ftlib_kinds.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature_gauss.o: ftlib_quadrature_gauss.f90 ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_quadrature.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature_trapezoidal.o: ftlib_quadrature_trapezoidal.f90 ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_quadrature.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature_rectangle.o: ftlib_quadrature_rectangle.f90 ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_quadrature.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature.o: ftlib_quadrature.f90 ftlib_kinds.o ftlib_function.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_root.o: ftlib_root.f90 ftlib_kinds.o ftlib_function.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_function.o: ftlib_function.f90 ftlib_kinds.o
	$(FC) $(FLAGS) -c $<

ftlib_io.o: ftlib_io.f90
	$(FC) $(FLAGS) -c $<

ftlib_kinds.o: ftlib_kinds.f90
	$(FC) $(FLAGS) -c $<

.PHONY: cleanlib
cleanlib:
	rm -rf ../include/* ../lib/* 
