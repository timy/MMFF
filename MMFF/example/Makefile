include ../Makefile.cfg

OUT = ../test
SRC_CONSOLE	= \
	console_master_int.f90 \
	console_slave_int.f90

FORTRAN_FLAGS	:= $(FORTRAN_FLAGS)
INCLUDE		:= $(INCLUDE) -I../src  # used to include the annoying module 'mmff_mpi_info'
LIBS		:= $(LIBS) -L.. -lmmff
MMFF_DIR	:= ./.
LINK_FLAGS	:= -m64 -Wl,-rpath,$(MMFF_DIR)


%.o : %.f90
	$(FC) $(FORTRAN_FLAGS) $(INCLUDE) $< -c -o $*.o 

%.o : %.f
	$(FC) $(FORTRAN_FLAGS) $(INCLUDE) $< -c -o $*.o 

OBJ_CONSOLE	= $(SRC_CONSOLE:.f90=.o)

all: build

build: $(OBJ_CONSOLE)
	$(FC) $(LINK_FLAGS) $(OBJ_CONSOLE) $(LIBS) -o $(OUT)

clean:
	$(RM) ./*.o
	$(RM) *.mod 
	$(RM) *~ 
	$(RM) $(OUT)

run:
	./$(OUT)
