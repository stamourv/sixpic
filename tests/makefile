# makefile for six-comp tests

.SUFFIXES:
.SUFFIXES: .c .hex .tmp .test

all: checks

.c.test:
	@echo ====================================================================== $*
	../../gambc-v4_2_9/bin/gsi-script ../six-comp.scm $*.c
	@rm -f $*.c.hex $*.c.tmp

checks: test1.test test2.test test3.test

clean:
	rm -f *.c.hex *.c.tmp
