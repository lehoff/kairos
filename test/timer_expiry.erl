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

-record(state,
        { servers = [] :: [chronos:server_name()],
          started = [] :: [{ server_timer(),
                             {time_stamp(),
                              chronos:timer_duration()}}],
          stopped = [] :: [ {server_timer(), time_stamp()} ],
          expired = [] :: [ {server_timer(), time_stamp()} ]
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

expire(ServerName, TimerName) ->
    gen_server:cast(?SERVER, {expire, {ServerName, TimerName}}).

timer_status(ServerName, TimerName) ->
    gen_server:call(?SERVER, {timer_status, {ServerName, TimerName}}).


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
    case lists:keyfind({Server,Timer}, 1, State#state.started) of
        false ->
            Reply =
                chronos:start_timer(Server, Timer, Duration,
                                    {timer_expiry, expire, [Server, Timer]}),
            {reply, Reply, State#state{started= [{{Server,Timer}, {Now, Duration}}
                                                 | State#state.started ]}};
        _ ->
            {reply,
             {error, "Starting the same timer twice - must be tested elsewhere"},
             State}
    end;
handle_call({stop_timer, {Server, Timer}}, _From, State) ->
    Reply = chronos:stop_timer(Server, Timer),
    Now = time_stamp(),
    {reply, Reply, State#state{stopped= [{{Server,Timer}, Now} | State#state.stopped ] }};
handle_call({timer_status, {_Server, _Timer}=Key}, _From, State) ->
    Reply =
        case keyfind(Key, State) of
            {false, false, false} ->
                not_started;
            {false, {_,_}, false} ->
                stopped_a_non_running;
            {{_, StartInfo}, false, false} ->
                {started, StartInfo};
            {{_, StartInfo}, {_, StopTime}, false} ->
                {stopped, {StartInfo, StopTime}};
            {{_, {StartTime, Duration}}, Stop, {_, ExpiryTime}} ->
                case Stop of
                    false ->
                        {expired, {StartTime, Duration, trunc((ExpiryTime - StartTime) / 1000)}};
                    {_, StopTime} when StopTime >= ExpiryTime ->
                        {expired, {StartTime, Duration, trunc((ExpiryTime - StartTime) / 1000)}};
                    {_, StopTime} ->
                        {expiry_after_stop, {StartTime, Duration, StopTime, ExpiryTime}}
                end;
            Other ->
                {error, {incorrect_timer_status, Other}}
        end,
    {reply, Reply, State}.


handle_cast({expire, {_Server, _Timer}=Key}, State) ->
    %% io:format("timer ~p expired~n", [Key]),
    Now = time_stamp(),
    {noreply, State#state{expired = [{Key, Now} | State#state.expired]}}.

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


keyfind(Key, #state{started=Started,
                    stopped=Stopped,
                    expired=Expired}) ->
    {lists:keyfind(Key, 1, Started),
     lists:keyfind(Key, 1, Stopped),
     lists:keyfind(Key, 1, Expired)}.
