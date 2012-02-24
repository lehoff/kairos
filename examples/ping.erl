%%%-------------------------------------------------------------------
%%% @author Torben Hoffmann <torben.lehoff@gmail.com>
%%% @copyright (C) 2012, Torben Hoffmann
%%% @doc Shows how to use chronos for timers.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ping).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([timer_expiry/1,
         ping/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        { tserver
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

timer_expiry(Timer) ->
    gen_server:call(?SERVER, {timer_expiry, Timer}).

ping() ->
    gen_server:call(?SERVER, ping).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    TServer = ping_timer_server,
    {ok, _Pid} = chronos:start_link(TServer),
    _TS = chronos:start_timer(TServer, ping_timer, 1000,
                              {?MODULE, timer_expiry, [ping_timer]}),
    {ok, #state{ tserver = TServer}}.

handle_call(ping, _From, State) ->
    io:format("Got ping~n",[]),
    _TS = chronos:start_timer(State#state.tserver, ping_timer, 1000,
                             {?MODULE, timer_expiry, [ping_timer]}),
    {reply, ok, State};
handle_call({timer_expiry,ping_timer}, _From, State) ->
    io:format("Got timer_expiry for ~p~n", [ping_timer]),
    _TS = chronos:start_timer(State#state.tserver,
                              silence_timer, 30000,
                              {?MODULE, timer_expiry, [silence_timer]}),
    {reply, ok, State};
handle_call({timer_expiry,silence_timer}, _From, State) ->
    io:format("Got timer_expiry for ~p~n", [silence_timer]),
    {stop, normal, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, State) ->
    io:format("~p terminating due to silence_timeout~n", [?SERVER]),
    ok = chronos:stop(State#state.tserver),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
