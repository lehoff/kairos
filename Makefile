#REBAR = ./rebar
REBAR = rebar
DIALYZER = dialyzer
CT_RUN = ct_run
TOUCH = touch

.PHONY: all deps compile escripize clean doc testclean eunit ct test release \
	devrelease_config devrelease devclean devconsole devconsole_clean \
	run plt analyze get-deps compile-deps distclean ct_setup tags \
	rebuild-yaws ct_run

all: deps compile

tags:
	find . -name "*.[he]rl" -print | etags -

deps: get-deps compile-deps

compile:
	@$(REBAR) compile skip_deps=true

escriptize:
	@$(REBAR) escriptize

clean: testclean
	@$(REBAR) clean
	@rm -f test/*.beam erl_crash.dump ./deps/.compile-deps
	@find . -name app.config -exec rm -f {} \;

#distclean: clean
#	@rm -fr apps/payment/logs

testclean:
	@rm -fr Mnesia.nonode\@nohost
	@rm -fr test/*.beam

eunit: compile testclean
	@$(REBAR) skip_deps=true eunit

test_build:
	erlc -I include  -o test test/*.erl

test_console:
	erl  -pa deps/*/ebin -pa ebin -pa test

ct_setup:
	mkdir -p logs

CWD = $(shell pwd)
ct_run:
	erl -noshell \
        -pa deps/*/ebin -pa apps/*/ebin \
	-sname ct \
	-env TEST_DIR test \
        -include ${CWD}/apps/payment/include \
	-spec backend-api.spec \
	-config backend-api.sys.config \
	-dir test \
	-s ct_run script_start -s erlang halt > logs/raw.log 2>&1

ct: ct_setup deps compile testclean ct_run
	sh ./test_ct_run.sh

test: ct

release: deps compile
	@rm -fr release/backend-api
	(cd release && ../rebar generate)

devrelease_config:
	@mkdir -p release/vars
	@cp release/vars.config release/vars/devrelease_vars.config

devrelease: deps compile devclean
	@mkdir -p dev
	(cd release \
	&& ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

devclean:
	@rm -fr dev

devconsole:
	./dev/devrelease/bin/backend-api console \
		-pa ../../apps/payment/ebin \
		-pa ../../deps/meck/ebin

devconsole_clean:
	./dev/devrelease/bin/backend-api console_clean \
		-pa ../../apps/payment/ebin \
		-pa ../../deps/meck/ebin

run: release
	release/backend-api/bin/backend-api console

rebuild-yaws:
	@echo 'Rebuilding some yaws beams with debug info'
	erlc +debug_info -I deps/yaws/include -o deps/yaws/ebin deps/yaws/src/*.erl

plt: rebuild-yaws
	$(DIALYZER) --build_plt --output_plt .backend-api.plt \
		-pa deps/*/ebin \
		deps/*/ebin \
		--apps kernel stdlib sasl inets crypto \
		public_key ssl mnesia runtime_tools erts \
		compiler tools syntax_tools xmerl hipe webtool

analyze: compile
	$(DIALYZER) --no_check_plt \
		     apps/*/ebin \
		--plt .backend-api.plt \
		-Werror_handling \
		-Wunmatched_returns #-Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true

get-deps:
	@$(REBAR) get-deps

compile-deps:
	@$(REBAR) compile
