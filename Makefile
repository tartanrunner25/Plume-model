PLUME=./

##
F_COMP=pgf90
F_OPTS=-O3 -Munroll=c:1 -Mnoframe -mcmodel=medium -Mlarge_arrays -Mlre -Mvect=cachesize:1048576,sse,prefetch \
##
#F_COMP=gfortran
#F_OPTS= -O0 -fcheck=bounds  -fbacktrace -fcheck=bounds -g -fcheck=all -finit-real=nan -Wall -ffpe-trap=invalid,zero
#
LOADER=${F_COMP}
LOADER_OPTS= -v

LIBS=

#ml pgi netcdf-c netcdf-f
LDFLAGS= -L$(NETCDFC_LIBDIR) -Wl,-rpath=$(NETCDFC_LIBDIR) -lnetcdf -L$(NETCDFF_LIBDIR) -Wl,-rpath=$(NETCDFF_LIBDIR) -lnetcdff
ARCHIVE=ar r

# Compiler commands.

F_COMMAND = $(F_COMP) -c $(F_OPTS)

# Define archive and executable names.

BASE=plume_alone_module
EXE=$(BASE)
ARC=$(BASE).a

# Define source.
F_SRC = \
  $(PLUME)/plume_alone_module.f90\
	$(PLUME)/read_namelist.f90\
	$(PLUME)/read_fsize.f90\
	$(PLUME)/read_finput.f90\
	$(PLUME)/find_metfile.f90\
	$(PLUME)/list_metfile.f90\
	$(PLUME)/netcdf_reader.f90\
	$(PLUME)/format_column_wrf.f90\
	$(PLUME)/write_plume.f90\
	$(PLUME)/run_plume_model.f90


# Define targets.

all: $(EXE)

$(EXE): $(ARC)
	$(LOADER) -o $(EXE) $(LOADER_OPTS) $(ARC) $(LDFLAGS)
###	ln -fs $(EXE) $(BASE)
	rm -f *.o core.*

$(ARC): $(F_SRC)
	$(F_COMMAND) $(?)
	$(ARCHIVE) $(ARC) *.o
	$(F_COMMAND)
	rm -f *.o core.*


install:
#	ln -fs `pwd`/$(EXE) ../run/$(BASE)
#	ln -fs `pwd`/$(EXE) ../test/$(BASE)

clean:
	rm -f $(ARC) $(EXE) $(BASE) *.o  ___* core* *.mod
