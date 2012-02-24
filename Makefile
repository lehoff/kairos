
build:
	sinan build

example_beams: examples/*.erl

examples: build example_beams
	erlc -o examples examples/*.erl

# this should be done smarter, but thta is what is there for now.
PROJ_PATH=-pa /apps/erlang/r14b4/lib/kernel-2.14.5/ebin -pa /apps/erlang/r14b4/lib/stdlib-1.17.5/ebin -pa /apps/erlang/r14b4/lib/compiler-4.7.5/ebin -pa /Users/th/Library/Erlang/lib/proper-1.0/ebin -pa /apps/erlang/r14b4/lib/eunit-2.2.1/ebin -pa /Users/th/git_repos/chronos/_build/chronos/lib/chronos-0.0.1/ebin -pa /Users/th/git_repos/meck/ebin
ex_shell:
	erl -pz examples ${PROJ_PATH}