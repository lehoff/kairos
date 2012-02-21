%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(timer_expiry).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/0]).



%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type server_timer() :: {chronos:server_name(),
                         chronos:timer_name()}.

-type time_stamp() :: pos_integer().

-type unique_timer() :: {server_timer(), time_stamp()}.

-export_type([server_timer/0,
              time_stamp/0,
              unique_timer/0
             ]).

-record(state,
        { servers   = [] :: [chronos:server_name()],
          started   = [] :: [ unique_timer() ],
          durations = [] :: [{unique_timer(),
                              chronos:timer_duration()}],
          restarted = [] :: [ {unique_timer(), time_stamp()} ],
          stopped   = [] :: [ {unique_timer(), time_stamp()} ],
          expired   = [] :: [ {unique_timer(), time_stamp()} ]
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

start_server(ServerName) ->
    gen_server:call(?SERVER, {start_server, ServerName}).

start_timer(ServerName, TimerName, Duration) ->
    gen_server:call(?SERVER, {start_timer,{ServerName, TimerName, Duration}}).

stop_timer(ServerName, TimerName) ->
    gen_server:call(?SERVER, {stop_timer,{ServerName, TimerName}}).

expire(ServerName, TimerName, StartTime) ->
    gen_server:cast(?SERVER, {expire, {{ServerName, TimerName}, StartTime}}).

timer_status(ServerName, TimerName, StartTimeStamp) ->
    gen_server:call(?SERVER, {timer_status, {{ServerName, TimerName}, StartTimeStamp}}).

last_timer_status(ServerName, TimerName) ->
    gen_server:call(?SERVER, {last_timer_status, {ServerName, TimerName}}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({start_server, ServerName}, _From, State) ->
    case chronos:start_link(ServerName) of
        {ok, _Pid} ->
            {reply, ok, State#state{servers = [ServerName | State#state.servers]}};
        Other ->
            {reply, Other, State}
    end;
handle_call({stop_server, Server}, _From, State) ->
    chronos:stop(Server),
    {reply, ok, State#state{servers = lists:delete(Server, State#state.servers)}};
handle_call({start_timer, {Server, Timer, Duration}}, _From, State) ->
    Now = time_stamp(),
    Reply =
        case chronos:start_timer(Server, Timer, Duration,
                                 {timer_expiry, expire, [Server, Timer, Now]}) of
            ok ->
                Now;
            Other ->
                Other
        end,
    State1 = State#state{started= [ {{{Server,Timer}, Now}, Duration}
                                          | State#state.started ],
                         durations = [ {{{Server,Timer}, Now}, Duration}
                                       | State#state.durations ]
                        },
    State2 =
        case last_start({Server, Timer}, State) of
            not_started ->
                State1;
            StartTime ->
                UniqueTimer = {{Server, Timer}, StartTime},
                State1#state{ restarted = [{UniqueTimer, Now}
                                           | State1#state.restarted] }
        end,
    {reply, Reply, State2};

handle_call({stop_timer, {Server, Timer}}, _From, State) ->
    case last_start({Server, Timer}, State) of
        not_started ->
            {reply, ok, State};
        StartTime ->
            UniqueTimer = {{Server, Timer}, StartTime},
            Reply = chronos:stop_timer(Server, Timer),
            Now = time_stamp(),
            {reply, Reply, State#state{stopped= [{UniqueTimer, Now} | State#state.stopped ] }}
    end;
handle_call({timer_status, UniqueTimer}, _From, State) ->
    Reply =
        timer_state(UniqueTimer, State),
    {reply, Reply, State};
handle_call({last_timer_status, ServerTimer}, _From, State) ->
    Reply =
        case last_start(ServerTimer, State) of
            not_started ->
                never_started;
            StartTime ->
                timer_state({ServerTimer, StartTime}, State)
        end,
    {reply, Reply, State}.


timer_state({_ServerTimer, StartTime}=UniqueTimer, State) ->
    case keyfind(UniqueTimer, State) of
        {false, false, false, false} ->
            not_started;
        {{_,Dur}, false , false, false} ->
            {started, Dur};
        {{_,_Dur}, false , {_,StopTime}, false} ->
            {stopped, StopTime};
        {{_,  Dur}, {_, RestartTime}, false, false} ->
            {restarted, {RestartTime, Dur}};
        {{_,  _Dur}, {_, RestartTime}, {_,StopTime}, false} ->
            {restarted_and_stopped, {RestartTime, StopTime}};
        {{_, Dur}, false, Stop, {_, ExpiryTime}} ->
            case Stop of
                false ->
                    {expired, {StartTime, Dur, trunc((ExpiryTime - StartTime) / 1000)}};
                {_, StopTime} when StopTime >= ExpiryTime ->
                    {expired, {StartTime, Dur, trunc((ExpiryTime - StartTime) / 1000)}};
                {_, StopTime} ->
                    {expiry_after_stop, {StartTime, Dur, StopTime, ExpiryTime}}
            end;
        Other ->
            {error, {incorrect_timer_status, Other}}
    end.


handle_cast({expire, UniqueTimer}, State) ->
    %% io:format("timer ~p expired~n", [Key]),
    Now = time_stamp(),
    {noreply, State#state{expired = [{UniqueTimer, Now} | State#state.expired]}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    [ chronos:stop(Server)
      || Server <- State#state.servers ],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


time_stamp() ->
    {Mega, Sec, Micro} = now(),
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.


keyfind(UniqueTimer, #state{durations=Durations,
                            restarted=Restarted,
                            stopped=Stopped,
                            expired=Expired}) ->

    {lists:keyfind(UniqueTimer, 1, Durations),
     lists:keyfind(UniqueTimer, 1, Restarted),
     lists:keyfind(UniqueTimer, 1, Stopped),
     lists:keyfind(UniqueTimer, 1, Expired)}.

last_start(ServerTimer, #state{started=Started}) ->
    StartTimes = [ Time || {ST, Time} <- Started,
                           ST == ServerTimer],
    case StartTimes == [] of
        true ->
            not_started;
        false ->
            lists:last(lists:sort(StartTimes))
    end.
