-module(chronos_SUITE).

-export([all/0,
         chronos_eqc/1]).

all() ->
    [chronos_eqc].

chronos_eqc(_) ->
    eqc:module(chronos_eqc).
