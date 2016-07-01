PROJECT = chronos

PROJECT_DESCRIPTION = chronos - a timer utility for Erlang
PROJECT_VERSION = 0.1.2

DEPS = gproc gen_leader

dep_gen_leader = git https://github.com/abecciu/gen_leader_revival.git master


TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck 0.8.2

CT_SUITES = chronos

include erlang.mk

test_chronos: ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

example_beams: examples/*.erl

examples: build example_beams
	erlc -o examples examples/*.erl


ex-shell: examples
	erl -pz examples -pz deps/*/ebin -pz ebin

clean-beam:
	rm -rf ebin/*.beam examples/*.beam test/*.beam


clean-ct:
	rm -rf ct_run*
	-rm -f all_runs.html
	-rm -f index.html
	-rm -f jquery*.js
	-rm -f ct_default.css
	-rm -f variables-ct*

deep-clean: clean-beam clean-ct
	rm -rf deps
