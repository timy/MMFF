MODULE_NAME	= libmmff
OUTPUT		= ../$(MODULE_NAME).so
OUTPUT_STATIC	= $(MODULE_NAME).a
DYNAMIC_DEP 	= $(OUTPUT)

SRC_LIBRARY 	= \
	write_log.f90 \
	dummy.f90 \
	mmff_base_dbl.f90 \
	mmff_workload.f90 \
	mmff_core.f90 \
	mmff_toolkit.f90 \
	mmff_main.f90
SRC_CONSOLE	= \
	console_master.f90 \
	console_slave.f90
FORTRAN_FLAGS 	=  -fPIC -I./src
FC		= /usr/local/Packages/mvapich2-tcp-intel-1.2rc2/bin/mpif90

LD_FLAGS       	= 
LIBS		=


%.o : %.f90
	$(FC) $(FORTRAN_FLAGS) $< -c -o $*.o 

%.o : %.f
	$(FC) $(FORTRAN_FLAGS) $< -c -o $*.o 


OBJ_LIBRARY	= $(SRC_LIBRARY:.f90=.o)
OBJ_CONSOLE	= $(SRC_CONSOLE:.f90=.o)
LD	  	= ld $(LD_FLAGS) -shared -fPIC



all: $(OBJ_LIBRARY) $(OBJ_CONSOLE)
	$(FC) $(OBJ_LIBRARY) $(OBJ_CONSOLE) -o ../app



build: $(DYNAMIC_DEP)




$(OUTPUT): $(OBJ_LIBRARY) 
	$(LD) $(LD_SHARED) -o $(OUTPUT) $(OBJ_LIBRARY) $(LIBS) $(LDFLAGS)
cleanall:
	rm *.o *~ *.mod *.so app

run:
	./app