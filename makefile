.SUFFIXES:
.SUFFIXES: .c .hex .tmp

all: sixpic

sixpic: six-comp.o1 pic18-sim.o1 utilities.o1 ast.o1 operators.o1 cte.o1 parser.o1 cfg.o1 optimizations.o1 code-generation.o1 register-allocation.o1

six-comp.o1: six-comp.scm
	rm $@ || true
	gsc -debug $<

pic18-sim.o1: pic18-sim.scm
	rm $@ || true
	gsc -debug $<

utilities.o1: utilities.scm
	rm $@ || true
	gsc -debug $<

ast.o1: ast.scm
	rm $@ || true
	gsc -debug $<

operators.o1: operators.scm
	rm $@ || true
	gsc -debug $<

cte.o1: cte.scm
	rm $@ || true
	gsc -debug $<

parser.o1: parser.scm
	rm $@ || true
	gsc -debug $<

cfg.o1: cfg.scm
	rm $@ || true
	gsc -debug $<

optimizations.o1: optimizations.scm
	rm $@ || true
	gsc -debug $<

code-generation.o1: code-generation.scm
	rm $@ || true
	gsc -debug $<

register-allocation.o1: register-allocation.scm
	rm $@ || true
	gsc -debug $<

# .scm.o1: # TODO doesn't work
# 	rm $@ || true
# 	gsc -debug $<

.c.hex:
	gsi six-comp.scm $*.c
	rm -f $*.c.tmp

checks:
	cd tests && make checks

clean:
	rm -rf *.c.tmp *.o1
