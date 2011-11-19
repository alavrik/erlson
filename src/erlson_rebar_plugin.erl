-module(erlson_rebar_plugin).

% rebar commands
-export([
    pre_compile/2,
    pre_eunit/2,
    pre_ct/2,
    pre_doc/2,
    pre_xref/2
]).


pre_compile(Config, X) ->
    common(Config, X).

pre_eunit(Config, X) ->
    common(Config, X).

pre_ct(Config, X) ->
    common(Config, X).

pre_doc(Config, X) ->
    common(Config, X).

pre_xref(Config, X) ->
    common(Config, X).


common(_Config, _X) ->
    erlson:init_erlc(),
    rebar_log:log(debug,
        "erlson_rebar_plugin: loaded erl_parse as ~s~n",
        [code:which(erl_parse)]),
    ok.

