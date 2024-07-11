include ../../src/Makefile

INC_DIR=../../include
LIB_DIR=../../lib
LIBS=-lftlib

main.e: main.f90
	$(FC) -I$(INC_DIR) -L$(LIB_DIR) $(FLAGS) $< -o main.e $(LIBS)

.PHONY: clean
clean:
	rm -rf main.e T.dat
