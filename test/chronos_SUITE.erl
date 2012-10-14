-module(chronos_SUITE).
-compile(export_all).

all() ->
    [chronos_eqc].

chronos_eqc(_) ->
    eqc:module(chronos_eqc).
