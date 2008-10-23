EFLAGS=-pa ebin

ERL := erl $(EFLAGS)

ERL_SOURCES := $(wildcard src/*.erl)

ERL_OBJECTS := $(ERL_SOURCES:src/%.erl=ebin/%.beam)


all: objects

objects: $(ERL_OBJECTS)

ebin/%.beam: src/%.erl
	@test -d ebin || mkdir ebin
	erlc -W +debug_info -o ebin $<

clean:
	rm -rf ebin/*.beam erl_crash.dump

test: objects
	$(ERL) -noshell -s rison_test all -s init stop

shell: objects
	@$(ERL)

dialyzer:
	dialyzer --src -c src/
