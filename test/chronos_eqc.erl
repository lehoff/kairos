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
         started = [] :: [ timer_expiry:unique_timer() ],
         running = [] :: [ timer_expiry:server_timer() ]
        }).

%% Initialize the state
initial_state() ->
    #state{
            servers = [],
            started = [],
            running = []
          }.

%% Command generator, S is the state
command(S) ->
    frequency
      ([ {100, {call, timer_expiry, start_server, [server_name()]}} ]
       ++ [ {1000, {call, timer_expiry, start_timer, start_timer_args(S)}}
            || S#state.servers /= [] ]
       ++ [ {200, {call, timer_expiry, stop_timer, stop_timer_args(S)}}
            || S#state.servers /= []  ]
       ++ [ {1000, {call, ?MODULE, advance_time, advance_time_args(S)}}
            || S#state.servers /= [] andalso possible_to_advance_time(S) ]
      ).

%% Next state transformation, S is the current state
next_state(S, _V, {call, timer_expiry, start_server, [ServerName]}) ->
    S#state{ servers = [ServerName | S#state.servers] };
next_state(S, TS, {call, timer_expiry, start_timer, [Server, Timer, _Duration]}) ->
    S#state{ started = [ {{Server,Timer}, TS} | S#state.started],
             running = [ {Server, Timer} | S#state.running ]
           };
next_state(S, {var,_}, {call, timer_expiry, stop_timer, [Server, Timer]}) ->
    S#state{ running = lists:delete({Server, Timer}, S#state.running)};
next_state(S, ok, {call, timer_expiry, stop_timer, [Server, Timer]}) ->
    S#state{ running = lists:delete({Server, Timer}, S#state.running) };
next_state(S, not_running, {call, timer_expiry, stop_timer, [_Server, _Timer]}) ->
    S;
next_state(S, _V, {call, ?MODULE, advance_time, [_Duration]}) ->
    S.

%% Precondition, checked before command is added to the command sequence
precondition(S, {call, timer_expiry, start_server, [ServerName]}) ->
    not lists:member(ServerName, S#state.servers);
precondition(S, {call, timer_expiry, start_timer, [Server, _Timer, Duration]}) ->
    lists:member(Server, S#state.servers) andalso
        Duration > 0;
precondition(S, {call, timer_expiry, stop_timer, [Server, _Timer]}) ->
    %% Key = {Server, Timer},
    %% orddict:is_key(Key, S#state.started) andalso
    %%     not lists:member(Key, S#state.stopped);
    lists:member(Server, S#state.servers);
precondition(S, {call, ?MODULE, advance_time, [_Duration]}) ->
    possible_to_advance_time(S).

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(_S, {call, timer_expiry, start_server, [_Server]}, ok) ->
    true;
postcondition(_S,
              {call, timer_expiry, start_timer, [_Server, _Timer, _Duration]},
              TS) when is_integer(TS) ->
    true;
postcondition(_S, {call, timer_expiry, stop_timer, [_Server, _Timer]}, ok) ->
    true;
postcondition(_S, {call, timer_expiry, stop_timer, [_Server, _Timer]}, {ok, _TimeLeft}) ->
    true;
postcondition(_S, {call, timer_expiry, stop_timer, [Server, Timer]}, {error,{not_running,Timer}}) ->
    valid_stop_timer_status({Server, Timer});
postcondition(S, {call, ?MODULE, advance_time, [_Duration]}, _) ->
    lists:all( fun valid_timer_status/1, all_timers(S) );
postcondition(_, _, _) ->
    false.

valid_timer_status({{Server, Timer}, StartTime}) ->
    case timer_expiry:timer_status(Server, Timer, StartTime) of
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
        {restarted, {RestartTime, Dur}}
          when (RestartTime - StartTime) < Dur * 1000 ->
            true;
        {restarted_and_stopped, {_RestartTime, _StopTime}} ->
            false
    end.

valid_stop_timer_status({Server, Timer}) ->
    case timer_expiry:last_timer_status(Server, Timer) of
        {expired, {_StartTime, Duration, ActualDuration}} when ActualDuration >= Duration->
            true;
        {expiry_after_stop, {StartTime, Duration, _StopTime, ExpiryTime}}
          when (ExpiryTime - StartTime) >= Duration * 1000->
            true;
        never_started ->
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

%% possible_to_stop_timer(S) ->
%%      not_stopped(S) /= [].

%% not_stopped(S) ->
%%     orddict:fetch_keys(S#state.started) -- S#state.stopped.

all_timers(S) ->
    orddict:to_list(S#state.started).

possible_to_advance_time(S) ->
    %% @todo should one check for durations of started timers here??
    S#state.running /= [].


%%-------------------- GENERATORS ------------------------------


server_name() -> {server, nat()}.

timer_name() -> {timer, nat()}.

timer_duration() -> choose(10, 100).


start_timer_args(S) ->
    ?LET(Server, oneof(S#state.servers),
         [Server, timer_name(), timer_duration()]).

stop_timer_args(S) ->
    ?LET({Server, Timer}, oneof( S#state.running ++ [{hd(S#state.servers), bogus}]),
         [Server,  Timer]).

advance_time_args(_S) ->
    [choose(5, 50)].

%%---------------------- OPERATIONS ----------------------

advance_time(Duration) ->
    timer:sleep(Duration).
