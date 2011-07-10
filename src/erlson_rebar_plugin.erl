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
    ErlParsePath = code:which(erl_parse),
    case {string:str(ErlParsePath, "erlson"), string:str(ErlParsePath, "stdlib")} of
        {N, 0} when N =/= 0 ->
            % Erlson version of the Erlang parser is already loaded
            ok;
        {0, N} when N =/= 0 ->
            % for loading the custom "erl_parse" module, we rely on the fact
            % that it is located next to this rebar plugin
            code:purge(erl_parse),
            code:delete(erl_parse),
            code:load_file(erl_parse),
            rebar_log:log(debug,
                "erlson_rebar_plugin: repointed erl_parse to: ~s~n",
                [code:which(erl_parse)]),
            true = (string:str(code:which(erl_parse), "erlson") =/= 0),
            ok
    end.

