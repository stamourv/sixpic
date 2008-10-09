# makefile for six-comp tests

.SUFFIXES:
.SUFFIXES: .c .hex .tmp .test

all: checks

.c.test:
	@echo ====================================================================== $*
	gsi-script ../six-comp.scm $*.c
	@rm -f $*.c.hex $*.c.tmp

checks: test1.test test2.test test3.test test4.test test5.test test8.test test9.test test10.test test11.test

clean:
	rm -f *.c.hex *.c.tmp
