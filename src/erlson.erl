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

-export([init/0, init_erlc/0]).

% public API
-export([from_proplist/1, from_nested_proplist/1, from_nested_proplist/2]).
-export([to_json/1, from_json/1]).
-export([list_to_json_array/1, list_from_json_array/1]).
% these two functions are useful, if there's a need to call mochijson2:decode
% and mochijson2:encode separately
-export([to_json_term/1, from_json_term/1]).

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


% @doc Create Erlson dictionary from a proplist
%
% During conversion, each atom() property is converted to {atom(), true}
% dictionary association.
-spec from_proplist/1 :: (List :: proplist()) -> orddict().

from_proplist(L) ->
    from_nested_proplist(L, _MaxDepth = 1).


% @doc Create nested Erlson dictionary from a (possibly nested) proplist
%
% During conversion, each atom() property is converted to {atom(), true}
% dictionary association.
-spec from_nested_proplist/1 :: (List :: proplist()) -> orddict().

from_nested_proplist(L) ->
    from_nested_proplist(L, _MaxDepth = 'undefined').


% @doc Create nested Erlson dictionary from a (possibly nested) proplist
%
% Valid nested proplists up to maximum depth of `MaxDepth` will be converted to
% Erlson dictionaries.
%
% `MaxDepth = 'undefined'` means no limit on the depth of nesting.
%
%
% ```
% from_nested_proplist(L) ->
%     from_nested_proplist(L, _MaxDepth = 'undefined').
%
% from_proplist(L) ->
%     from_nested_proplist(L, _MaxDepth = 1).
% '''
-spec from_nested_proplist/2 :: (
    List :: proplist(),
    MaxDepth :: 'undefined' | pos_integer()) -> orddict().

from_nested_proplist(L, MaxDepth) ->
    try from_proplist_1(L, init_max_depth(MaxDepth))
    catch
        'erlson_bad_proplist' ->
            erlang:error('erlson_bad_proplist', [L])
    end.


init_max_depth('undefined') -> % no limit on the depth of nesting
    % by starting from 0 we'll never hit depth 0 when decrementing MaxDepth in
    % from_proplist_1
    0;
init_max_depth(X) -> X. % positive integer


from_proplist_1(L, MaxDepth) when is_list(L) ->
    % inserting elements to the new orddict() one by one
    % XXX: use merge sort instead of insertion sort?
    lists:foldl(
        fun (X, Dict) -> store_proplist_elem(X, Dict, MaxDepth - 1) end,
        _EmptyDict = [],
        L
    );
from_proplist_1(_, _) ->
    throw('erlson_bad_proplist').


store_proplist_elem({N, V}, Dict, MaxDepth) when is_atom(N) ->
    Value =
        case MaxDepth of
            0 -> V; % we've riched the maximum nesting depth
            _ ->
                % if property value is a valid property list, convert it to a
                % nested dictionary
                try from_proplist_1(V, MaxDepth)
                catch 'erlson_bad_proplist' -> V
                end
        end,
    store_val(N, Value, Dict);

store_proplist_elem(X, Dict, _MaxDepth) when is_atom(X) ->
    store_val(X, true, Dict);

store_proplist_elem(_X, _Dict, _MaxDepth) ->
    throw('erlson_bad_proplist').


% @doc Convert Erlson dictionary to a JSON Object
-spec to_json/1 :: (Dict :: orddict()) -> iolist().
to_json(Dict) ->
    JsonStruct = to_json_term(Dict),
    mochijson2:encode(JsonStruct).


% @doc Convert Erlson dictionary to JSON abstract term representation
%
% The JSON term representation can be converted to JSON iolist() by calling
% mochijson2:encode/1
to_json_term(Dict) ->
    try to_json_struct(Dict)
    catch
        'erlson_bad_json' ->
            erlang:error('erlson_bad_json', [Dict])
    end.


% @doc Convert a list of Erlson dictionaries to a JSON array
-spec list_to_json_array/1 :: (List :: [orddict()]) -> iolist().
list_to_json_array(List) ->
    JsonStruct = list_to_json_term(List),
    mochijson2:encode(JsonStruct).


% @doc Convert list of Erlson dictionaries to list of JSON abstract terms
list_to_json_term(List) ->
    try [encode_json_term(X) || X <- List]
    catch
        'erlson_bad_json' ->
            erlang:error('erlson_bad_json', [List])
    end.



to_json_struct(Dict) when is_list(Dict) ->
    Fields = lists:map(fun to_json_field/1, Dict),
    {'struct', Fields};
to_json_struct(_) ->
    throw('erlson_bad_json').


to_json_field({N, V}) when is_atom(N); is_binary(N) ->
    {N, encode_json_term(V)};
to_json_field(_) ->
    throw('erlson_bad_json').


encode_json_term('undefined') -> 'null';
encode_json_term(X) when
        is_binary(X); is_atom(X); % JSON string
        is_integer(X); is_float(X); % JSON number
        is_boolean(X) -> % JSON true | false
    X;
encode_json_term([]) -> [];
encode_json_term(L) when is_list(L) ->
    % try to treat list as a nested dictionary
    try to_json_struct(L)
    catch 'erlson_bad_json' ->
        % otherwise, encode as JSON array
        [ encode_json_term(X) || X <- L ]
    end;
encode_json_term(_) ->
    throw('erlson_bad_json').


% @doc Create Erlson dictionary from JSON Object
-spec from_json/1 :: (Json :: iolist()) -> orddict().
from_json(Json) ->
    JsonTerm = mochijson2:decode(Json),
    from_json_term(JsonTerm).


% @doc Create Erlson dictionary from JSON abstract term representation
%
% The JSON term representation can be obtained from JSON iolist() by calling
% mochijson2:decode/1
from_json_term(JsonTerm = {'struct', _Fields}) ->
    decode_json_term(JsonTerm);
from_json_term(JsonTerm) ->
    erlang:error('erlson_json_struct_expected', [JsonTerm]).


% @doc Create list of Erlson dictionaries from JSON array
-spec list_from_json_array/1 :: (Json :: iolist()) -> [orddict()].
list_from_json_array(Json) ->%, list_to_json_array, list_from_json_term and list_to_json_term
    JsonTerm = mochijson2:decode(Json),
    list_from_json_term(JsonTerm).


% @doc Create list of Erlson dictionaries from list of JSON abstract terms
list_from_json_term(JsonTerm) when is_list(JsonTerm) ->
    decode_json_term(JsonTerm);
list_from_json_term(JsonTerm) ->
    erlang:error('erlson_json_array_expected', [JsonTerm]).


decode_json_term(X) when
        is_binary(X); % JSON string
        is_integer(X); is_float(X); % JSON number
        is_boolean(X) -> % JSON true | false
    X;
decode_json_term({'struct', Fields}) -> % JSON object
    from_json_fields(Fields);
decode_json_term(L) when is_list(L) -> % JSON array
    [ decode_json_term(X) || X <- L ];
decode_json_term('null') ->
    % decoding JSON null as a more conventional 'undefined'
    'undefined'.


from_json_fields(L) ->
    % inserting elements to the new orddict() one by one
    % XXX: use merge sort instead of insertion sort?
    lists:foldl(fun store_json_field/2, _EmptyDict = [], L).


store_json_field({N, V}, Dict) ->
    Name = decode_json_field_name(N),
    Value = decode_json_term(V),
    store_val(Name, Value, Dict).


% the way Erlson treats field names is important. Each field can be represented
% as either atom() or binary(), and because Erlson dict is ordered, all binary()
% fields will be stored closer to the tail of the list
decode_json_field_name(N) ->
    try binary_to_existing_atom(N, utf8)
    catch
        error:badarg -> N
    end.


% @doc Enable Erlson syntax in Erlang shell
init() ->
    init(erl_parse_shell).


% @doc Enable Erlson syntax when compiling Erlang modules using erlc. The
% resulting Erlang AST will be proccessed by Erlson pase transform module
% (erlson_pt.erl).
init_erlc() ->
    init(erl_parse_erlc).


init(Mod) ->
    % unload stdlib's erl_parse
    code:purge(erl_parse),
    code:delete(erl_parse),

    code:add_path(code:lib_dir(erlson, priv)),
    case code:get_object_code(Mod) of
        {_, Code, File} ->
            code:unstick_dir(filename:dirname(File)),
            case code:load_binary(erl_parse, File, Code) of
                {module, _Name} -> ok;
                {error, Reason} ->
                    exit({erlson_error,
                        {"failed to load module " ++ atom_to_list(Mod), Reason}})
            end;
        error ->
            exit({erlson_error,
                    "failed to load code from module " ++ atom_to_list(Mod)})
    end.

