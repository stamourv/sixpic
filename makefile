# makefile for six-comp

.SUFFIXES:
.SUFFIXES: .c .hex .tmp

all: checks

.c.hex:
	gsi six-comp.scm $*.c
	rm -f $*.c.tmp

checks:
	cd tests && make checks

clean:
	rm -f *.c.tmp
