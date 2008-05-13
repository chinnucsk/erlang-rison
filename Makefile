SHELL=/bin/sh

EFLAGS=-pa ebin

all: compile

compile:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make

clean:
	rm -rf ebin erl_crash.dump

test: compile
	erl $(EFLAGS) -noshell -eval 'ok = rison_test:all(), c:q().'

i: compile
	erl $(EFLAGS) -eval 'c:l(rison).'