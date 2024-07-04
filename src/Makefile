FC=gfortran
FLAGS=-Wall -g -fcheck=all -cpp -DVERBOSE

LIBOBJS= ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_root.o ftlib_quadrature.o \
ftlib_quadrature_rectangle.o ftlib_quadrature_trapezoidal.o ftlib_quadrature_gauss.o  ftlib_grid.o \
 ftlib_grid1d.o ftlib_grid2d.o ftlib_scalar1d.o ftlib_scalar2d.o ftlib_polynomials.o ftlib_fields.o

lib: $(LIBOBJS)
	ar r libftlib.a $(LIBOBJS); mv libftlib.a ../lib; rm *.o; mv *.mod ../include; mv *.smod ../include


ftlib_polynomials.o: ftlib_polynomials.f08 ftlib_kinds.o ftlib_grid2d.o ftlib_function.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_fields.o: ftlib_fields.f08 ftlib_scalar1d.o
	$(FC) $(FLAGS) -c $<

ftlib_scalar2d.o: ftlib_scalar2d.f08 ftlib_kinds.o ftlib_grid2d.o
	$(FC) $(FLAGS) -c $<

ftlib_scalar1d.o: ftlib_scalar1d.f08 ftlib_kinds.o ftlib_grid1d.o
	$(FC) $(FLAGS) -c $<

ftlib_grid2d.o: ftlib_grid2d.f08 ftlib_kinds.o ftlib_grid.o
	$(FC) $(FLAGS) -c $<

ftlib_grid1d.o: ftlib_grid1d.f08 ftlib_kinds.o ftlib_grid.o
	$(FC) $(FLAGS) -c $<

ftlib_grid.o: ftlib_grid.f08 ftlib_kinds.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature_gauss.o: ftlib_quadrature_gauss.f08 ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_quadrature.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature_trapezoidal.o: ftlib_quadrature_trapezoidal.f08 ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_quadrature.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature_rectangle.o: ftlib_quadrature_rectangle.f08 ftlib_kinds.o ftlib_function.o ftlib_io.o ftlib_quadrature.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_quadrature.o: ftlib_quadrature.f08 ftlib_kinds.o ftlib_function.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_root.o: ftlib_root.f08 ftlib_kinds.o ftlib_function.o ftlib_io.o
	$(FC) $(FLAGS) -c $<

ftlib_function.o: ftlib_function.f08 ftlib_kinds.o
	$(FC) $(FLAGS) -c $<

ftlib_io.o: ftlib_io.f08
	$(FC) $(FLAGS) -c $<

ftlib_kinds.o: ftlib_kinds.f08
	$(FC) $(FLAGS) -c $<

.PHONY: cleanlib
cleanlib:
	rm -rf ../include/* ../lib/* 