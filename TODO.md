# To-Dos for Chronos


## Removal of timers for terminating requesters

When the requester of a timer dies it should be possible to get the timers that has been requested removed.

This is only a problem if more processes share the same Chronos timer server, since a single process which has started a Chronos timer serevr will drag Chronos down with it when it dies.


## Flexible linking to the creating process

Right now a Chronos timer server can only be started with `start_link/1` which means that it will die when the creator of it dies.

It might be a good thing to decouple this, but since I have not found a good use case for this yet it is not implemented.

## Support for recurring timers

If you want to have a recurring timer, say every 5 seconds, you have to start a new timer every time it expires.

If this is too troublesome to do it will be possible to introduce recurring timers - I encourage users of Chronos to try it out as it is today and provide me with some information about how recurring timers should work for them. Then I will implement the feature.
