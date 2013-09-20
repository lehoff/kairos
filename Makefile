PROJECT = chronos

DEPS = gproc edown gen_leader

dep_gproc = git://github.com/uwiger/gproc 0.2.13.3
dep_edown = https://github.com/esl/edown.git master
dep_gen_leader = https://github.com/abecciu/gen_leader_revival.git master


TEST_DEPS = meck

dep_meck = git://github.com/eproxus/meck 0.7.2

include erlang.mk



test: test/*.erl build
	ct_run -dir test -pa ebin -pa deps/*/ebin

example_beams: examples/*.erl

examples: build example_beams
	erlc -o examples examples/*.erl


ex_shell: examples
	erl -pz examples -pz deps/*/ebin -pz ebin

clean_beam:
	rm -rf ebin/*.beam examples/*.beam test/*.beam
