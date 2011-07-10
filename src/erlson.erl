%%  Copyright (c) 2011 Anton Lavrik, http://github.com/alavrik
%%  
%%  Permission is hereby granted, free of charge, to any person obtaining
%%  a copy of this software and associated documentation files (the
%%  "Software"), to deal in the Software without restriction, including
%%  without limitation the rights to use, copy, modify, merge, publish,
%%  distribute, sublicense, and/or sell copies of the Software, and to
%%  permit persons to whom the Software is furnished to do so, subject to
%%  the following conditions:
%%  
%%  The above copyright notice and this permission notice shall be
%%  included in all copies or substantial portions of the Software.
%%  
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%%  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%%  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%%  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

%
% Erlson runtime library
%
-module(erlson).

-export([init/0]).
-export([fetch/2, store/3]).


% dictionary represented as an ordered list of name-value pairs
-type orddict() :: [ {name(), value()} ].
-type name() :: atom() | binary().
-type path() :: atom() | [ atom() ].
-type value() :: name().


-spec fetch/2 :: (
    Path :: path(),
    Dict :: orddict() ) -> value().

fetch(Path, Dict) ->
    try
        case is_atom(Path) of
            true -> fetch_val(Path, Dict);
            false -> fetch_path(Path, Dict)
        end
    catch
        'erlson_not_found' ->
            erlang:error('erlson_not_found', [Path, Dict])
    end.


fetch_path([H|T], Dict) ->
    Val = fetch_val(H, Dict),
    fetch_path(T, Val);
fetch_path([], Val) -> Val.


fetch_val(Name, [{N, _V} | T]) when Name > N ->
    fetch_val(Name, T);
fetch_val(Name, [{N, V} | _T]) when Name =:= N -> V;
fetch_val(_Name, _) ->
    not_found().


not_found() ->
    throw('erlson_not_found').


-spec store/3 :: (
    Path :: path(),
    Value :: any(),
    Dict :: orddict() ) -> orddict().

store(Name, Value, Dict) when is_atom(Name) ->
    store_val(Name, Value, Dict);
store(Path, Value, Dict) ->
    try
        store_path(Path, Value, Dict)
    catch
        'erlson_not_found' ->
            erlang:error('erlson_not_found', [Path, Dict])
    end.


store_path([N], Value, Dict) ->
    store_val(N, Value, Dict);
store_path([H|T], Value, Dict) ->
    InnerDict = fetch_val(H, Dict),
    % replace the existing value with the new inner dictionary
    NewInnerDict = store_path(T, Value, InnerDict),
    store_val(H, NewInnerDict, Dict).


store_val(Name, Value, Dict) ->
    orddict:store(Name, Value, Dict).


% Enable Erlson syntax in Erlang shell
init() ->
    case code:get_object_code(erl_parse_shell) of
        {_, Code, File} ->
            code:unstick_dir(filename:dirname(File)),
            case code:load_binary(erl_parse, File, Code) of
                {module, _Name} -> ok;
                {error, Reason} ->
                    exit({erlson_error,
                        {"failed to load erl_parse_shell.beam", Reason}})
            end;
        error ->
            exit({erlson_error,
                    "failed to load code from erl_parse_shell.beam"})
    end.

