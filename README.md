# Chronos - a timer utility for Erlang.

Erlang comes with some good utilities for timers, but there are some
shortcomings that might become an issue for you as they did for me. 

Chronos tries to hide the book keeping from the user in a way that
will be very familiar to those who have tried to implement protocols
from the telecommunications realm. 

In addition to the abstraction the Chronos distribution also shows how
to design the APIs of your code in such a way that it makes it easier
to test the code. This part requires the use of the meck application
by Adam Lindberg or some serious manual hacking... I am going to show
the meck way of doing it.

Before describing how to use the Chronos timers I will provide a brief
overview of what the existing timer solutions has to offer so you can
make an informed choice about which timer solution fits your problem
the best.

# Existing timer solutions

## The timer module from the stdlib

This is an excellent module in terms of abstraction: it provides all
the functionality you would want in order to start and stop timers.

Unfortunately there can only be one timer server for each Erlang node
and that is that it can very easily become overloaded - see the
[http://erlang.org/doc/efficiency_guide/commoncaveats.html] entry on
the timer module.

## Using the erlang modules timer functions

As the Efficiency Guide states the erlang:send_after/3 and
erlang:start_timer/3 are much more efficient than the timer module,
but using them directly requires some book keeping which can clutter
your code uncessarily.

## Using the OTP timers

I am assuming that you use Erlang/OTP to develop your software - if not you can skip this
section! And in that case you probably never got to this line since
the Chronos abstraction is too high level for your taste...

For gen_server and gen_fsm you can specify a timer in the result tuple
from your handler functions, e.g., for gen_fsm you can return
{next_state,NextStateName,NewStateData,Timeout}. If the gen_server does not receive
another message within Timeout milliseconds a timeout event will
happen. The problem with this approach is that if any message arrives
the timer is cancelled and in many cases you want to see a specific
message before you cancel the timer or you want to have multiple
timers running.

You can have multiple timers with gen_fsm by using the
gen_fsm:start_timer/2 function. The down side is that you have to do
book keeping of timer references if you want to cancel the timer. This
is similar to using the erlang modules timers mentioned above.

Hmmm, perhaps the OTP timers can be used without much fuzz for this
after all... just a matter of style.


