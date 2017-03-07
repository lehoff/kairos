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
                    [ #api_fun{ name = start_timer, arity=3 },
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
    ?CALLOUT(chronos_command, start_timer, [Duration, TimerName, callback()], Ref).

start_new_timer_next(S, _, [Server, Timer, _Duration, Ref]) ->
    S#state{ started = [ {{Server,Timer}, Ref} | S#state.started],
             running = [ {Server, Timer} | S#state.running ]}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_running_timer(Server, Timer) ->
    chronos:stop_timer(Server, Timer).

stop_running_timer_args(S) ->
    oneof( S#state.running ).

stop_running_timer_pre(S) ->
    S#state.running /= [].

%% stop_running_timer_post(_S, _Args, ok) ->
%%     true;
%% stop_running_timer_post(_S, _Args, not_running) ->
%%     false.

stop_running_callouts(S, [Server, Timer]) ->
    Key = {Server, Timer},
    Ref = proplists:get_value(Key, S#state.started),
    ?CALLOUT(chronos_command, cancel_timer, [Ref], 10).

stop_running_timer_next(S, _Res, [Server, Timer]) ->
    S#state{ running = lists:delete({Server, Timer}, S#state.running) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%stop_bogus_timer(Server) ->



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

%%-------------------- GENERATORS ------------------------------


server_name() -> 
    ?LET(N, nat(),
         begin
             Str = lists:flatten(io_lib:format("server~p", [N])),
             list_to_atom(Str)
         end).

timer_name() -> {timer, nat()}.

timer_duration() -> choose(10, 100).




