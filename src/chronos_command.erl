-module(chronos_command).


-export([start_timer/2,
         cancel_timer/1,
         execute_callback/1]).


-spec start_timer(non_neg_integer, chronos:timer_name()) -> 
                         'ok' | {'error', term()}.
start_timer(Duration, TimerName) ->
    erlang:start_timer(Duration, self(), TimerName).

-spec cancel_timer(reference()) -> non_neg_integer() | 'false'.
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

-spec execute_callback(chronos:callback()) -> 'ok'.
execute_callback({Mod, Fun, Args}) ->
% We spawn a function to execute the apply because we want to protect the timer
% server against errors. Might need to log this.
    spawn ( fun() ->
                    erlang:apply(Mod, Fun, Args)
            end ).
            
                                             
                         
