# SIBERIA makefile to make application

# NOTE: This makefile assumes that the source files are in the directory one level up
#       and that objects, .mod files and the executables are to be put in the current
#       directory. This is done to facilitate multiplatform code. The common fortran and C 
#       code is in a common directory and the platform dependent executables, etc are
#       are in a directory that is unique to that platform, operating system and CPU.
#
#   Platform Dependencies
#   ---------------------
#       On Mac OSX the executable directory looks a little messy because there is also an XCode
#       project ... but its doesn't use the makefile ... we only use XCode for editing.

#  ensure that the compilation proceeds in the order specified in OBJECTS
#  so that module interfaces are compiled in correct order (simplier than defining
#  their file dependencies).

.NOTPARALLEL:

#  Note this make file assumes that the variable 'f95' is defined in the shell (typically in
#   .bashrc in BASH shell or the corresponding file for whatever shell you use)

#F95COMPILER=ifort
F95COMPILER=g95
#F95COMPILER=gfortran

TYPE = siberia

RELEASE_LOCATION = release

VERSION = 833

OBJECTS = siberia-modules.o support.o openmp-module.o siberiasetup.o others.o   \
          ctrout.o inout.o layerconstants.o layers.o hydrology.o areaanal.o     \
          channel.o mymodels.o control.o                                        \
          user-dir.o user-erosion.o user-other.o user-runoff.o user-uplift.o    \
          diranal.o montecarlo.o pitanal.o pvm-dummy.o sedanal.o rsu.o          \
          soilanal.o siberia-compute.o siberia.o

# RULE TO CREATE EXECUTABLE
# -------------------------


#  standard UNIX version
#$(TYPE)_$(VERSION) : $(OBJECTS)
#        f90 -o $(TYPE)_$(VERSION) $(OBJECTS)
#  SGI scalar version
#$(TYPE)_$(VERSION) : $(OBJECTS)
#        f90 -Ofast -o $(TYPE)_$(VERSION) $(OBJECTS)
#   SGI openMP version
#$(TYPE)_$(VERSION) : $(OBJECTS)
#	f90 -mp -Ofast -o $(TYPE)_$(VERSION) $(OBJECTS)
#   G95 version 
$(TYPE)_$(VERSION) : $(OBJECTS)
	$(F95COMPILER) -O2 -o $(RELEASE_LOCATION)/$(TYPE)_$(VERSION) $(OBJECTS)


#  RULE TO COMPILE C CODE
#  ----------------------

%.o:../%.c
	cc -c  ../$*.c

#  RULE TO COMPILE FORTRAN CODE
#  ----------------------------


# LINUX
#.f.o.:
#	f77 -c -O $*.f
# SGI options (scalar version)
#.f90.o:
#	f90 -Ofast -c -extendsource  $*.f90
# SGI options (openMP version)
#.f90.o:
#	f90 -mp -Ofast -c -extendsource $*.f90
# SUN options
#.f.o:
#	f77 -c -e -fast -O4  $*.f
#
# OSX G95 options
%.o:../%.f90
	$(F95COMPILER) -c -O2 ../$*.f90

