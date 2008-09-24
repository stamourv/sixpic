# makefile for six-comp tests

.SUFFIXES:
.SUFFIXES: .c .hex .tmp .test

all: checks

.c.test:
	@echo ====================================================================== $*
	gsi ../six-comp.scm $*.c
	@rm -f $*.c.hex $*.c.tmp

checks: test1.test test2.test

clean:
	rm -f *.c.hex *.c.tmp
