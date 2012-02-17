%%% @author Torben Hoffmann <th@issuu.com>
%%% @copyright (C) 2012, Torben Hoffmann
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2012 by Torben Hoffmann <th@issuu.com>

-module(chronos_eqc).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state,
        {servers = [] :: [chronos:server_name()],
         started = [] :: [{{chronos:server_name(), chronos:timer_name()},
                           chronos:timer_duration()}],
         stopped = [] :: [{chronos:server_name(), chronos:timer_name()}]
        }).

%% Initialize the state
initial_state() ->
    #state{
            servers = [],
            started = orddict:new(),
            stopped = []
          }.

%% Command generator, S is the state
command(S) ->
    frequency
      ([ {100, {call, timer_expiry, start_server, [server_name()]}} ]
       ++ [ {1000, {call, timer_expiry, start_timer, start_timer_args(S)}}
            || S#state.servers /= [] ]
       ++ [ {200, {call, timer_expiry, stop_timer, stop_timer_args(S)}}
            || S#state.servers /= [] andalso possible_to_stop_timer(S) ]
       ++ [ {1000, {call, ?MODULE, advance_time, advance_time_args(S)}}
            || S#state.servers /= [] andalso possible_to_advance_time(S) ]
      ).

%% Next state transformation, S is the current state
next_state(S, _V, {call, timer_expiry, start_server, [ServerName]}) ->
    S#state{ servers = [ServerName | S#state.servers] };
next_state(S, _V, {call, timer_expiry, start_timer, [Server, Timer, Duration]}) ->
    S#state{ started = orddict:store({Server,Timer}, Duration, S#state.started) };
next_state(S, _V, {call, timer_expiry, stop_timer, [Server, Timer]}) ->
    S#state{ stopped = [{Server, Timer} | S#state.stopped]};
next_state(S, _V, {call, ?MODULE, advance_time, [_Duration]}) ->
    S.

%% Precondition, checked before command is added to the command sequence
precondition(S, {call, timer_expiry, start_server, [ServerName]}) ->
    not lists:member(ServerName, S#state.servers);
precondition(S, {call, timer_expiry, start_timer, [Server, Timer, Duration]}) ->
    lists:member(Server, S#state.servers) andalso
        not orddict:is_key({Server,Timer}, S#state.started) andalso
        Duration > 0;
precondition(S, {call, timer_expiry, stop_timer, [Server, Timer]}) ->
    Key = {Server, Timer},
    orddict:is_key(Key, S#state.started) andalso
        not lists:member(Key, S#state.stopped);
precondition(S, {call, ?MODULE, advance_time, [_Duration]}) ->
    possible_to_advance_time(S).

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(_S, {call, timer_expiry, start_server, [_ServerName]}, ok) ->
    true;
postcondition(_S, {call, timer_expiry, start_timer, [_Server, _Timer, _Duration]}, ok) ->
    true;
postcondition(_S, {call, timer_expiry, stop_timer, [_Server, _Timer]}, ok) ->
    true;
postcondition(_S, {call, timer_expiry, stop_timer, [Server, Timer]}, {error,{not_running,Timer}}) ->
    valid_stop_timer_status({Server, Timer});
postcondition(S, {call, ?MODULE, advance_time, [_Duration]}, _) ->
    lists:all( fun valid_timer_status/1, not_stopped(S) );
postcondition(_, _, _) ->
    false.

valid_timer_status({Server, Timer}) ->
    case timer_expiry:timer_status(Server, Timer) of
        {expired, {_StartTime, Duration, ActualDuration}} when ActualDuration >= Duration->
            true;
        {expiry_after_stop, {StartTime, Duration, _StopTime, ExpiryTime}}
          when (ExpiryTime - StartTime) >= Duration * 1000->
            true;
        {started, _} ->
            true;
        {stopped, {StartTime, Duration, StopTime}}
          when (StopTime - StartTime) < Duration * 1000 ->
            true;
        _Other ->
            %%            io:format("timer_status = ~p~n", [Other]),
            false
    end.

valid_stop_timer_status({Server, Timer}) ->
    case timer_expiry:timer_status(Server, Timer) of
        {expired, {_StartTime, Duration, ActualDuration}} when ActualDuration >= Duration->
            true;
        {expiry_after_stop, {StartTime, Duration, _StopTime, ExpiryTime}}
          when (ExpiryTime - StartTime) >= Duration * 1000->
            true;
        _Other ->
            %%            io:format("timer_status = ~p~n", [Other]),
            false
    end.


prop_chronos() ->
    ?FORALL(Cmds,commands(?MODULE),
            ?TRAPEXIT(
               begin
                   start_context(),
                   {H,S,Res} = run_commands(?MODULE,Cmds),
                   stop_context(),
                   ?WHENFAIL(
                      io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
                   Res == ok)
               end)).

start_context() ->
    application:start(gproc),
    timer_expiry:start_link().

stop_context() ->
    timer_expiry:stop(),
    application:stop(gproc).


%%-------------------- ASSERTIONS ----------------------

possible_to_stop_timer(S) ->
     not_stopped(S) /= [].

not_stopped(S) ->
    orddict:fetch_keys(S#state.started) -- S#state.stopped.

possible_to_advance_time(S) ->
    %% @todo should one check for durations of started timers here??
    not_stopped(S) /= [].


%%-------------------- GENERATORS ------------------------------


server_name() -> {server, nat()}.

timer_name() -> {timer, nat()}.

timer_duration() -> choose(10, 100).


start_timer_args(S) ->
    ?LET(Server, oneof(S#state.servers),
         [Server, timer_name(), timer_duration()]).

stop_timer_args(S) ->
    ?LET({Server, Timer}, oneof( orddict:fetch_keys(S#state.started) -- S#state.stopped ),
         [Server,  Timer]).

advance_time_args(_S) ->
    [choose(5, 50)].

%%---------------------- OPERATIONS ----------------------

advance_time(Duration) ->
    timer:sleep(Duration).
