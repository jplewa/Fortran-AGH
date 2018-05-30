PFUNIT = /opt/pfunit/pfunit-serial
F90_VENDOR = Intel

include $(PFUNIT)/include/base.mk

FFLAGS += -std08 -funroll-loops -I$(PFUNIT)/mod
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

PFS = $(wildcard *.pf)
OBJS = $(PFS:.pf=.o)

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

%.o: ../src/%.F90
	$(F90) $(FFLAGS) -c ./$<

test: testSuites.inc ./mult.o $(OBJS)
	$(F90) -o ../bin/$@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -I../src \
		$(PFUNIT)/include/driver.F90 \
		./*$(OBJ_EXT) $(LIBS) $(FFLAGS)