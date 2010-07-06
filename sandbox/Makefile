SUBDIRS = src

all build cleanall: $(SUBDIRS)
	for subdir in $(SUBDIRS); do (cd $${subdir}; $(MAKE) $@); done 

run:
	./app
#	qsub run.sh  -q opt.q

clean:
	rm app *.so *~ MPI_test.* LOG