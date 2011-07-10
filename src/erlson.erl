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

% public API
-export([to_list/1, from_list/1]).

% these functions are used by Erlson compiled code
-export([fetch/2, store/3]).


% dictionary represented as an ordered list of name-value pairs
-type orddict() :: [ {name(), value()} ].

% binary() names are allowed, but only atom() names are reacheable (see below)
-type name() :: atom() | binary().

% any value can be stored in Erlson dictionary
-type value() :: any().

% a path to reachable (possibly nested) orddict() elements
-type name_path() :: atom() | [ atom() ].


% proplists of restricted format can be loaded into Erlson object
% proplist() is a supertype of orddict() type
-type proplist() :: [ proplist_elem() ].
-type proplist_elem() :: atom() | {atom(), value()}.


-spec fetch/2 :: (
    Path :: name_path(),
    Dict :: orddict() ) -> value().

fetch(Path, Dict) ->
    try
        case is_atom(Path) of
            true -> fetch_val(Path, Dict);
            false ->
                % NOTE: this branch is not currently in use
                fetch_path(Path, Dict)
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
    Path :: name_path(),
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


-spec to_list/1 :: (Dict :: orddict()) -> orddict().
to_list(Dict) -> Dict.


% @doc Convert an Erlson dictionary from a (possibly nested) proplist
%
% During conversion, each atom() property is converted to {atom(), true}
% dictionary association.
-spec from_list/1 :: (List :: proplist()) -> orddict().

from_list(L) ->
    try from_list_1(L)
    catch
        'erlson_bad_list' ->
            erlang:error('erlson_bad_list', [L])
    end.


from_list_1(L) when is_list(L) ->
    % inserting elements to the new orddict() one by one
    lists:foldl(fun store_proplist_elem/2, _EmptyDict = [], L);
from_list_1(_) ->
    throw('erlson_bad_list').


store_proplist_elem(X, Dict) when is_atom(X) ->
    store_val(X, true, Dict);
store_proplist_elem({N, V}, Dict) when is_atom(N) ->
    Value =
        % if property value is a valid property list, convert it to a nested
        % dictionary
        try from_list_1(V)
        catch 'erlson_bad_list' -> V
        end,
    store_val(N, Value, Dict);

store_proplist_elem(_X, _Dict) ->
    throw('erlson_bad_list').


% @doc Enable Erlson syntax in Erlang shell
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

