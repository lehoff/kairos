
build:
	rebar compile

test: test/*.erl build
	ct_run -dir test -pa ebin -pa deps/*/ebin

example_beams: examples/*.erl

examples: build example_beams
	erlc -o examples examples/*.erl


ex_shell: examples
	erl -pz examples -pz deps/*/ebin -pz ebin

clean:
	rm -rf ebin/*.beam examples/*.beam test/*.beam
