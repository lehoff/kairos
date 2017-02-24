-module(ping_test).

-compile(export_all).

setup() ->
    meck:new(chronos, [passthrough]),
    meck:expect(chronos, start_timer,
                fun(_,_,_,_) ->
                        0
                end).


start_test() ->
    {ok, _Pid} = ping:start_link(),
    true = meck:called(chronos, start_link, '_').

ping_timer_started_test() ->
    true = meck:called(chronos, start_timer,%% '_').
                       ['_', '_',
                        1000,
                        {ping, timer_expiry, [ping_timer]}]).

expire_ping_timer_test() ->
    ping:timer_expiry(ping_timer),
    true = meck:called(chronos, start_timer, ['_', '_',
                                              30000,
                                              {ping, timer_expiry, [silence_timer]}]).

expire_silence_timer_test() ->
    ping:timer_expiry(silence_timer),
    timer:sleep(100),
    undefined = whereis(ping),
    ok.



run_test() ->
    setup(),
    start_test(),
    ping_timer_started_test(),
    expire_ping_timer_test(),
    expire_silence_timer_test().
