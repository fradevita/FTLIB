include ../../src/Makefile

INC_DIR=../../include
LIB_DIR=../../lib
LIBS=-lftlib -llapack

test.e: test.f08
	$(FC) -I$(INC_DIR) -L$(LIB_DIR) $(FLAGS) $< -o test.e $(LIBS)

.PHONY: clean
clean:
	rm -rf test.e