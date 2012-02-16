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
        {servers = [],
         timers = [] :: [{chronos:server_name(), chronos:timer_name()}]
        }).

%% Initialize the state
initial_state() ->
    #state{
            servers = [],
            timers = []
          }.

%% Command generator, S is the state
command(S) ->
    frequency( [ {100, {call, timer_expiry, start_server, [server_name()]}} ]
           ).

%% Next state transformation, S is the current state
next_state(S,_V,{call,timer_expiry, start_server, [ServerName]}) ->
    S#state{ servers = [ServerName | S#state.servers] }.

%% Precondition, checked before command is added to the command sequence
precondition(S,{call,timer_expiry, start_server, [ServerName]}) ->
    not lists:member(ServerName, S#state.servers).

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(_S,{call,timer_expiry, start_server, [_ServerName]}, ok) ->
    true;
postcondition(_S,{call,timer_expiry, start_server, [_ServerName]}, _Other) ->
    false.

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



%%-------------------- GENERATORS ------------------------------


server_name() -> {server, nat()}.

timer_name() -> {timer, nat()}.

timer_timeout() -> choose(10, 100).

%%---------------------- OPERATIONS ----------------------

start_server(ServerName) ->
    chronos:start_link(ServerName).

start_timer(Server, Timer, Timeout) ->
    ok.
