%%% @copyright (C) 2012-2017, Torben Hoffmann

-module(chronos_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-record(state,
        {servers = [] :: [chronos:server_name()],
         started = [] :: [ {chronos:server_name(), chronos:timer_name(), reference()} ],
         running = [] :: [ {chronos:server_name(), chronos:timer_name()} ]
        }).

%% Initialize the state
initial_state() ->
    #state{
            servers = [],
            started = [],
            running = []
          }.

api_spec() ->
    #api_spec{
       language = erlang,
       modules = 
           [ #api_module{
                name = chronos_command,
                functions = 
                    [ #api_fun{ name = start_timer, arity=2 },
                      #api_fun{ name = cancel_timer, arity=1 },
                      #api_fun{ name = execute_callback, arity=1 } ]}
           ]}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_server() ->
    {ok, Pid} = chronos:start_link(),
    Pid.

start_server_args(_S) ->
    [].


start_server_next(S, Pid, []) ->
    S#state{servers = [Pid | S#state.servers]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_new_timer(Server, TimerName, Duration, _Ref) ->
    chronos:start_timer(Server, TimerName, Duration, callback()).

start_new_timer_process(_, _) ->
    worker.

callback() ->
    {io, format, ["Hello World"]}.

start_new_timer_args(S) ->
    ?LET(Server, oneof(S#state.servers),
         [Server, timer_name(), timer_duration(), make_ref()]).

start_new_timer_pre(S) ->
    S#state.servers /= [].

start_new_timer_pre(S, [Server, TimerName, Duration, _Ref]) ->
    lists:member(Server, S#state.servers) andalso
        not lists:member({Server, TimerName}, S#state.running) andalso
        Duration > 0.

start_new_timer_callouts(_S, [_Server, TimerName, Duration, Ref]) ->
    ?CALLOUT(chronos_command, start_timer, [Duration, TimerName], Ref).

start_new_timer_next(S, _, [Server, Timer, _Duration, Ref]) ->
    S#state{ started = [ {{Server,Timer}, Ref} | S#state.started],
             running = [ {Server, Timer} | S#state.running ]}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_running_timer(Server, Timer) ->
    chronos:stop_timer(Server, Timer).

stop_running_timer_args(S) ->
    ?LET({Server, Timer}, oneof( S#state.running ),
         [Server, Timer]).

stop_running_timer_pre(S) ->
    S#state.running /= [].

stop_running_timer_post(_S, _Args, {ok,_}) ->
    true;
stop_running_timer_post(_S, _Args, not_running) ->
    false.

stop_running_timer_callouts(S, [Server, Timer]) ->
    Key = {Server, Timer},
    Ref = proplists:get_value(Key, S#state.started),
    ?CALLOUT(chronos_command, cancel_timer, [Ref], 10).

stop_running_timer_next(S, _Res, [Server, Timer]) ->
    S#state{ running = lists:delete({Server, Timer}, S#state.running) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_bogus_timer(Server) ->
    chronos:stop_timer(Server, bogus).

stop_bogus_timer_args(S) ->
    [oneof(S#state.servers)].

stop_bogus_timer_pre(S) ->
    S#state.servers /= [].

stop_bogus_timer_post(_S, _Args, not_running) ->
    true;
stop_bogus_timer_post(_S, _Args, {ok, _}) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_chronos() ->
    ?SETUP( fun setup/0,
            ?FORALL(Cmds,commands(?MODULE),
                    ?TRAPEXIT(
                       begin
                           {H,S,Res} = run_commands(?MODULE,Cmds),
                           stop(S),
                           ?WHENFAIL(
                              io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
                              Res == ok)
                       end))).

setup() ->
    eqc_mocking:start_mocking(api_spec()),
    fun teardown/0.

teardown() ->
    eqc_mocking:stop_mocking().

stop(S) ->
    [ chronos:stop(Server) || Server <- S#state.servers ].



prop_executes() ->
    ?FORALL(Duration, timer_duration(),
            begin
                chronos:start_link(chronos_test),
                ExpiryDuration = check(Duration),
                ExpiryDuration >= (Duration*1.0)
            end).

check(Duration) ->
    Self = self(), 
    Ref = make_ref(),
    Pid = spawn( fun () -> checker(Self, Ref) end),
    chronos:start_timer(chronos_test, some_name, Duration, {?MODULE, timer_expiry, [Pid]}),
    Pid ! start,
    receive
        {duration, Ref, ActualDuration} ->
            io:format("~p : ~p~n ", [ActualDuration, Duration]),
            ActualDuration
    after Duration * 3 ->
            error
    end.

timer_expiry(Pid) ->
    Pid ! expiry.

checker(From, Ref) ->
    receive
        start ->
            Start = erlang:timestamp(),
            checker2(From, Ref, Start)
    end.

checker2(From, Ref, Start) ->
    receive
        expiry ->
            End = erlang:timestamp(),
            Delta = ms_delta(Start, End),
            From ! {duration, Ref, Delta}
    end.
        

ms_delta({Mega1, Sec1, Micro1}, {Mega2, Sec2, Micro2}) ->
    {Mega, Sec, Micro} = {Mega2 - Mega1, Sec2 - Sec1, Micro2 - Micro1},
    (1000000*Mega + Sec) * 1000 + Micro / 1000.

%%-------------------- GENERATORS ------------------------------


server_name() -> 
    ?LET(N, nat(),
         begin
             Str = lists:flatten(io_lib:format("server~p", [N])),
             list_to_atom(Str)
         end).

timer_name() -> {timer, nat()}.

timer_duration() -> choose(10, 100).




