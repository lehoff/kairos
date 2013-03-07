.PHONY: build get-deps update-deps test clean deep-clean

REBAR = rebar

build:
	@$(REBAR) compile

test: test/*.erl build
	ct_run -dir test -pa ebin -pa deps/*/ebin

example_beams: examples/*.erl

examples: build example_beams
	erlc -o examples examples/*.erl


ex_shell: examples
	erl -pz examples -pz deps/*/ebin -pz ebin

clean:
	@$(REBAR) clean

deep-clean: clean
	@$(REBAR) delete-deps

get-deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps
