include ./Makefile.cfg

SUBDIRS = src $(MAIN_DIR)

all: clean $(SUBDIRS)
	for subdir in $(SUBDIRS); do (cd $${subdir}; $(MAKE) $@); done 

run:
	qsub run.sh  -q opt.q

clean:
	cd $(MAIN_DIR) && $(MAKE) $@
	$(RM) *~ 
	$(RM) MPI_test.* 
	$(RM) LOG
	$(RM) *.dat

cleanall: clean src
	cd $(SUBDIRS) && $(MAKE) $@