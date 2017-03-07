# chronos - a timer utility for Erlang.

Erlang comes with some good utilities for timers, but there are some
shortcomings that might become an issue for you as they did for me.

chronos tries to hide the book keeping from the user in a way that
will be very familiar to those who have tried to implement protocols
from the telecommunications realm.

In addition to the abstraction the chronos distribution also shows how
to design the APIs of your code in such a way that it makes it easier
to test the code. This part requires the use of the meck application
by Adam Lindberg or some serious manual hacking... I am going to show
the meck way of doing it. See the `ping_test` module in the `examples`
directory.

Abstracting time as it is suggested (with or with chronos) will give
you a design where you can test how timers work very fast. You can
trust things will work in real life since chronos comes with a test
suite that shows that it does what you would expect. The burden on you
is then to write a test that shows that your component works as it
should for a sequence of events where some of them happens to be
timers expiring. And you do not have to wait for the timers to expire
since time has been abstracted.

Below the description of how to use chronos there is a brief
overview of what the existing timer solutions has to offer so you can
make an informed choice about which timer solution fits your problem
the best.


# The chronos approach to timers

The design of chronos was influenced by the problems with the existing
timer solutions and shaped by the needs when implementing telecom
protocols, which uses timers extensively.

## Timer servers

Instead of having a single global timer server chronos allows you to
create as many or as few timer serves as you see fit.

    start_link(ServerName)

will start a new timer server where

    ServerName :: atom().

## Timers

Once you have started a timer server you can start timers using

    start_timer(ServerName, TimerName, Timeout, Callback)

where

    ServerName :: atom() | pid().
    TimerName :: term()
    Timeout :: pos_integer()
    Callback :: {module(), atom(), [term[]]}

The timer will be given a unique name - if you start the timer again
it will be restarted - and when the `Timeout` ms has passed the
`Callback` function will be called.

In some cases you want to cancel a timer and for that you can use

`stop_timer(ServerName, TimerName)`

In this case the `Callback` function will not be called.

chronos keeps track of all the running timers so your application code
can concentrate on what it is supposed to do without having to do
tedious book keeping.

## Testing with chronos using meck

Getting rid of tedious book keeping is not the only thing you get from
using chronos. By putting all your timers in the hands of chronos you
get a set-up that is very easy to mock so that you can abstract time
out of your tests.

One way of doing this is to provide a `timer_expiry` function as part
of the API for the component you are creating.  One of the arguments
should be the name of the timer and if you start more instances of the
component you need to have the name of the component in the call as
well.

    timer_expiry(TimerName) ->
        gen_server:call(?SERVER, {timer_expiry, TimerName}).

In the code you can request a timer like this:

    chronos:start_timer(ServerName,
                        timer_4,
                        {my_mod, timer_expiry, [timer_4]})

and then handling the timeout becomes very simple:

    handle_call({timer_expiry, timer_4}, _From, State) ->
        ...

That is the basic set-up and while testing you have to mock
chronos. This is easy to do with the meck application and should be
quite simple to do with any mocking library.

So you ensure that you have control over chronos:

    meck:new(chronos),
    meck:expect(chronos, start_timer,
                fun(_,_) -> 42 end)

As part of the test you check that the timer was requsted to start:

    meck:called(chronos, start_timer, [my_server, timer_4])

And when you come to the point in the test where you want to see the
effects of the timer expiry you simply call

    my_mod:timer_expiry(timer_4)

This approach lends itself well to property based testing and unit
testing.

## Testing with chronos using EQC mocking

The approach is the same as with meck, the only things you need to change is the
mocking of chronos.

The most common way of using mocking with EQC is through the `eqc_component`, where
you have to specify an `api_spec/1` function:

    api_spec() ->
        #api_spec{
            language = erlang,
            modules = [
                #api_module{
                    name = chronos,
                    functions = [
                        #api_fun{
                            name = start_link, arity = 1},
                        #api_fun{
                            name = start_link, arity = 0},
                        #api_fun{
                            name = start_timer, arity = 4},
                        #api_fun{
                            name = stop_timer, arity = 2} ]}]}.

You can then specify the callouts to these in the `_callouts/2` function for a
command:

    my_command_callouts(S, Args) ->
        ?CALLOUT(chronos, start_timer, [ServerName, TimerName, Duration, MFA], ok).
        
This will give you a very precise description of all aspects of the protocol that
your component is following. Yes, timers are part of the protocol.

# Existing timer solutions

## The timer module from the stdlib

This is an excellent module in terms of abstraction: it provides all
the functionality you would want in order to start and stop timers.

Unfortunately there can only be one timer server for each Erlang node
and that is that it can very easily become overloaded - see the
[http://erlang.org/doc/efficiency_guide/commoncaveats.html] entry on
the timer module.

## Using the erlang modules timer functions

As the Efficiency Guide states the `erlang:send_after/3` and
`erlang:start_timer/3` are much more efficient than the timer module,
but using them directly requires some book keeping which can clutter
your code uncessarily.

## Using the OTP timers

I am assuming that you use Erlang/OTP to develop your software - if
not you can skip this section! And in that case you probably never got
to this line since the chronos abstraction is too high level for your
taste...

For `gen_server` and `gen_fsm` you can specify a timer in the result
tuple from your handler functions, e.g., for `gen_fsm` you can return
`{next_state,NextStateName,NewStateData,Timeout}`. If the `gen_server`
does not receive another message within `Timeout` milliseconds a timeout
event will happen. The problem with this approach is that if any
message arrives the timer is cancelled and in many cases you want to
see a specific message before you cancel the timer or you want to have
multiple timers running.

You can have multiple timers with `gen_fsm` by using the
`gen_fsm:start_timer/2` function. The down side is that you have to do
book keeping of timer references if you want to cancel the timer. This
is similar to using the erlang module's timers mentioned above.

The downside is that there is not equivalent of
`gen_fsm:start_timer/2` for `gen_server` so for that you have to use
one of the other solutions.

# Linking approach

The chronos timer server is designed to be used as a process that is owned by one process.
There are a number of reasons for this:

* When a timer server dies you want to be notified and take appropriate action.
* When the starting process dies you want the timer server to go away.

The former can be dealt with by simple supervision, but the fixing the latter would
require much more complexity in the chronos code.

If an arbitrary number of process link to the timer server you need to protect the
timer server against the possible death of any of them. 
This can be done by adding a layer on top of chronos that deals with this. Hence, in
order to keep chronos simple this has deliberately been left out. Also, so far no one
has come up with a use case where sharing the timer server is required. This leads me
to believe that such cases will have special traits leading to the need for custom
code in each case.

# Roadmap

Based on the use cases chronos has been used for so far it seems that it is a useful
little utility, that does not need a huge additional feature set.

So by the end of Q2-2017 the 1.0 version will be released unless unforeseen features
emerges from concrete use cases.


# Installing chronos

## Using erlang.mk

Just add
```
dep_chronos = https://github.com/lehoff/chronos <version>
```
to your Makefile and it should be fine.


## Using rebar
If you are using rebar to build your project you should add the following to your dependencies:
```
    {chronos, "0.0.3", {git, "git://github.com/lehoff/chronos.git", {tag, <version>}}}
```

# FAQ

## Q: How can you be sure that the timers will do the right thing?

A: chronos comes with a property based testing suite that validates
   that the timers can be started, stopped and restarted as expected
   and that they expire as expected.
