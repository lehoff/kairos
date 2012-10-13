
build:
	rebar compile

example_beams: examples/*.erl

examples: build example_beams
	erlc -o examples examples/*.erl


ex_shell:
	erl -pz examples -pz deps/*/ebin -pz ebin
