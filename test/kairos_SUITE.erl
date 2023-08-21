-module(kairos_SUITE).

-export([all/0,
         kairos_eqc/1]).

all() ->
    [kairos_eqc].

kairos_eqc(_) ->
    eqc:module(kairos_eqc).
