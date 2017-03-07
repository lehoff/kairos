%%% @copyright (C) 2012-2017, Torben Hoffmann

-module(chronos_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-record(state,
        {
         started = [] :: [ {chronos:server_name(), chronos:timer_name(), reference()} ],
         running = [] :: [ {chronos:server_name(), chronos:timer_name()} ]
        }).

%% Initialize the state
initial_state() ->
    #state{
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


weight(_S, stop_bogus_timer) -> 1;
weight(_S, start_new_timer) -> 2;
weight(_S, _) -> 8.
                                 
    
server_name() ->
    chronos_server.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_new_timer(TimerName, Duration, _Ref) ->
    chronos:start_timer(server_name(), TimerName, Duration, callback()).

callback() ->
    {io, format, ["Hello World"]}.

start_new_timer_args(_S) ->
    [timer_name(), timer_duration(), make_ref()].


start_new_timer_pre(S, [TimerName, Duration, _Ref]) ->
    not lists:keymember( TimerName, 1, S#state.running) andalso
        Duration > 0.

start_new_timer_callouts(_S, [TimerName, Duration, Ref]) ->
    ?CALLOUT(chronos_command, start_timer, [Duration, TimerName], Ref).

start_new_timer_next(S, _, [Timer, _Duration, Ref]) ->
    S#state{ started = [ {Timer, Ref} | S#state.started],
             running = [ {Timer, Ref} | S#state.running ]}.
    
start_new_timer_features(_S, _Args, _Res) ->
    [start_new_timer].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_running_timer(Timer, _TRef) ->
    chronos:stop_timer(server_name(), Timer).

stop_running_timer_args(S) ->
    ?LET({Timer, TRef}, oneof( S#state.running ),
         [Timer, TRef]).

stop_running_timer_pre(S) ->
    S#state.running /= [].

stop_running_timer_post(_S, _Args, {ok,_}) ->
    true;
stop_running_timer_post(_S, _Args, not_running) ->
    false.

stop_running_timer_callouts(_S, [_Timer, TRef]) ->
    ?CALLOUT(chronos_command, cancel_timer, [TRef], 10).

stop_running_timer_next(S, _Res, [Timer, TRef]) ->
    S#state{ running = lists:delete({Timer, TRef}, S#state.running) }.

stop_running_timer_features(_S, _Args, _Res) ->
    [stop_running_timer].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_bogus_timer() ->
    chronos:stop_timer(server_name(), bogus).

stop_bogus_timer_args(_S) ->
    [].

stop_bogus_timer_post(_S, _Args, not_running) ->
    true;
stop_bogus_timer_post(_S, _Args, {ok, _}) ->
    false.

stop_bogus_timer_features(_S, _Args, _Res) ->
    [stop_bogus_timer].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
timer_timeout(TimerName, TRef) ->
    server_name() ! {timeout, TRef, TimerName}.

timer_timeout_args(S) ->
    ?LET({TimerName, TRef}, oneof(S#state.running),
         [TimerName, TRef]).

timer_timeout_pre(S) ->
    S#state.running /= [].

timer_timeout_pre(S, [TimerName, TRef]) ->
    lists:member({TimerName, TRef}, S#state.running).

timer_timeout_callouts(_S, _Args) ->
    ?CALLOUT(chronos_command, execute_callback, [callback()], ok).

timer_timeout_next(S, _Res, [TimerName, TRef]) ->
    S#state{
      running = lists:delete({TimerName, TRef}, S#state.running)}.
    
timer_timeout_features(_S, _Args, _Res) ->
    [timer_timeout].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_chronos() ->
    ?SETUP( fun setup/0,
            ?FORALL(Cmds, with_parameter(default_process, worker, commands(?MODULE)),
                    ?TRAPEXIT(
                       begin
                           start(),
                           {H,S,Res} = run_commands(?MODULE,Cmds),
                           stop(S),
                           pretty_commands(?MODULE, Cmds, {H, S, Res},
                                           aggregate(call_features(H),
                                                     Res == ok))
                       end))).

setup() ->
    eqc_mocking:start_mocking(api_spec()),
    fun teardown/0.

teardown() ->
    eqc_mocking:stop_mocking().

start() ->
    chronos:start_link(server_name()).

stop(_S) ->
    chronos:stop(server_name()).

ms_delta({Mega1, Sec1, Micro1}, {Mega2, Sec2, Micro2}) ->
    {Mega, Sec, Micro} = {Mega2 - Mega1, Sec2 - Sec1, Micro2 - Micro1},
    (1000000*Mega + Sec) * 1000 + Micro / 1000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_executes() ->
    ?FORALL(Duration, timer_duration(),
            begin
                chronos:start_link(server_name()),
                ExpiryDuration = check(Duration),
                chronos:stop(server_name()),
                ExpiryDuration >= (Duration*1.0)
            end).

check(Duration) ->
    Self = self(), 
    Pid = spawn( fun () -> checker(Self) end),
    chronos:start_timer(server_name(), some_name, Duration, {?MODULE, timer_expiry, [Pid]}),
    Pid ! start,
    receive
        {duration, ActualDuration} ->
            ActualDuration
    after Duration * 3 ->
            error
    end.

timer_expiry(Pid) ->
    Pid ! expiry.

checker(From) ->
    receive
        start ->
            Start = erlang:timestamp(),
            await_expiry_and_reply(From, Start)
    end.

await_expiry_and_reply(From, Start) ->
    receive
        expiry ->
            End = erlang:timestamp(),
            Delta = ms_delta(Start, End),
            From ! {duration, Delta}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_restart() ->
    ?FORALL({Duration, CancelDuration}, timer_duration_cancel(),
            begin
                chronos:start_link(server_name()),
                ExpiryDuration = check_restart(Duration, CancelDuration),
                chronos:stop(server_name()),
                ExpiryDuration >= (Duration + CancelDuration)*1.0
            end).


check_restart(Duration, CancelDuration) ->
    Self = self(),
    Pid = spawn( fun() -> restart_checker(Self, Duration, CancelDuration) end), 
    chronos:start_timer(server_name(), some_name, Duration, {?MODULE, timer_expiry, [Pid]}),
    Pid ! start,
    receive
        {duration, ActualDuration} ->
            ActualDuration
    after 
        Duration*2 + CancelDuration ->
            error
    end.
                          

restart_checker(From, Duration, CancelDuration) ->
    receive 
        start ->
            Start = erlang:timestamp(),
            timer:sleep(CancelDuration),
            %% @todo: should we check the remaining time?
            chronos:start_timer(server_name(), some_name, Duration, {?MODULE, timer_expiry, [self()]}),
            await_expiry_and_reply(From, Start)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_cancel() ->
    ?FORALL({Duration, CancelDuration}, timer_duration_cancel(),
            begin
                chronos:start_link(server_name()),
                Res = listen_after_cancel(Duration, CancelDuration),
                chronos:stop(server_name()),
                eqc:equals(Res, nothing_received)
            end).

listen_after_cancel(Duration, CancelDuration) ->        
    Self = self(),
    Pid = spawn( fun() -> cancel_and_listen(Self, Duration, CancelDuration) end),
    chronos:start_timer(server_name(), some_name, Duration, {?MODULE, timer_expiry, [Pid]}),
    Pid ! start,
    receive
        {listen_result, Res} ->
            Res
    end.
                         
cancel_and_listen(From, Duration, CancelDuration) ->
    receive
        start ->
            timer:sleep(CancelDuration),
            chronos:stop_timer(server_name(), some_name),
            listen(From, 2*Duration - CancelDuration)
    end.

listen(From, ListenPeriod) ->
    Res = 
        receive
            Msg ->
                {error, Msg}
        after 
            ListenPeriod ->
                nothing_received
        end,
    From ! {listen_result, Res}.
            


%%-------------------- GENERATORS ------------------------------

timer_name() -> {timer, nat()}.

timer_duration() -> choose(10, 100).

timer_duration_cancel() ->
    ?LET(Duration, timer_duration(),
         {Duration, choose(1,Duration-2)}).




