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

pre_xref(Config, X) ->
    common(Config, X).

pre_doc(Config, X) ->
    % we have to load 'erl_parse_shell' (by calling erlson:init()) instead of
    % 'erl_parse_erlc`, because, unlike erlc (used by "rebar compile"), edoc
    % (used by "rebar doc") doesn't apply parse transformations and therefore
    % can't recognize Erlson syntax elements
    erlson:init(),
    common_1(Config, X).


common(Config, X) ->
    erlson:init_erlc(),
    common_1(Config, X).


common_1(_Config, _X) ->
    rebar_log:log(debug,
        "erlson_rebar_plugin: loaded erl_parse as ~s~n",
        [code:which(erl_parse)]),
    ok.

