%%  Copyright (c) 2010, 2011 Anton Lavrik, http://github.com/alavrik
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

-module(erl_aliases).
-export([parse_transform/2]).


% "Abstract Format" chapter from ERTS User Guide
% http://www.erlang.org/doc/apps/erts/absform.html 
%
% Testing:
%       compile:file("t.erl", 'P').
%       compile:file("t.erl", 'E').


% uncomment this like to print some debug information
%-define(DEBUG,1).

-ifdef(DEBUG).
-compile(export_all).
-define(PRINT(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(PRINT(Fmt, Args), ok).
-endif.


parse_transform(Forms, _Options) ->
    try
        lists:flatmap(fun rewrite/1, Forms)
    catch
        %{error, Es} -> exit(Es);
        {error, Es, Line} ->
            File = get_file(),
            Error = {File,[{Line,compile,{parse_transform,?MODULE,Es}}]},
            {error, [Error], []};
        {missing_rule, Term} ->
            Es = lists:flatten(io_lib:format(
                "missing transformation rule for: ~w, "
                "stack ~w", [Term, erlang:get_stacktrace()])),
            exit(Es)
    end.


set_file(File) ->
    put('__erl_aliases_file__', File).

get_file() ->
    get('__erl_aliases_file__').


% check wither a key is present in the dictionary
is_key(Name, DictName) ->
    case get(DictName) of
        'undefined' -> false; % there is no such dictionary
        Dict -> dict:is_key(Name, Dict)
    end.

% store a key-value pair in the dictionary; create an empty dictionary if it
% hasn't been created before
store(Name, Value, DictName) ->
    Dict =
        case get(DictName) of
            'undefined' -> dict:new();
            X -> X
        end,
    NewDict = dict:store(Name, Value, Dict),
    put(DictName, NewDict),
    ok.


set_dict_alias(Name, Line) ->
    case is_record_alias_name(Name) of
        false -> ok;
        true ->
            Es = lists:concat([
                "dict alias ", Name,
                " conflicts with a previously defined record alias"]),
            throw({error, Es, Line})
    end,
    case is_record_name(Name) of
        false -> ok;
        true ->
            Es_1 = lists:concat([
                "dict alias ", Name,
                " conflicts with a previously defined record"]),
            throw({error, Es_1, Line})
    end,
    case get_dict_alias() of
        0 -> ok; % undefined
        _ ->
            Es_2 = "dict alias has been already defined; "
                   "only one is allowed per module",
            throw({error, Es_2, Line})
    end,
    put('__erl_aliases_dict__', Name).

get_dict_alias() ->
    case get('__erl_aliases_dict__') of
        'undefined' -> 0; % this value won't match (see below)
        X -> X
    end.

is_dict_alias(Name) -> get_dict_alias() == Name.


is_record_name(Name) ->
    is_key(Name, '__erl_aliases_records__').

add_record_name(Name, Line) ->
    case is_record_alias_name(Name) of
        false -> ok;
        true ->
            Es = lists:concat([
                "record ", Name,
                " conflicts with a previously defined record alias"]),
            throw({error, Es, Line})
    end,
    case is_dict_alias(Name) of
        false -> ok;
        true ->
            Es_1 = lists:concat([
                "record ", Name,
                " conflicts with a previously defined dict alias"]),
            throw({error, Es_1, Line})
    end,
    % we don't need any value to be associated with a record name at the
    % moment
    % XXX: store the location?
    DictName = '__erl_aliases_records__',
    store(Name, _Value = 'undefined', DictName).


add_record_alias(_Name, '', Line) ->
    % Explicitly prohibiting use of '' as an alias, because:
    %   - it conflicts with dict syntax;
    %   - at the same time, it is not universal -- unlike for dicts, it can't
    %     be applied only to one record at once (or even per module).
    Es = "use of '' as alias is prohibited",
    throw({error, Es, Line});

add_record_alias(Name, Alias, Line) ->
    case is_record_name(Alias) of
        false -> ok;
        true ->
            Es = lists:concat([
                "record alias ", Alias,
                " conflicts with a previously defined record"]),
            throw({error, Es, Line})
    end,
    case is_dict_alias(Alias) of
        false -> ok;
        true ->
            Es_1 = lists:concat([
                "record alias ", Alias,
                " conflicts with a previously defined dict alias"]),
            throw({error, Es_1, Line})
    end,
    add_alias('__erl_record_aliases__', Name, Alias, Line).

is_record_alias_name(Name) ->
    is_key(Name, '__erl_record_aliases__').

unalias_record(Name) ->
    unalias('__erl_record_aliases__', Name).


add_module_alias(Name, Alias, Line) ->
    add_alias('__erl_module_aliases__', Name, Alias, Line).

unalias_module(Name) ->
    unalias('__erl_module_aliases__', Name).


unalias_module_atom({'atom', LINE, Name}) ->
    Name1 = unalias_module(Name),
    {'atom', LINE, Name1};
unalias_module_atom(X) -> X.


% add record or module alias entry to the correspondent dictionary
add_alias(_DictName, Name, Alias, Line) when Name == Alias ->
    Es = lists:concat(["alias ", Alias, " is an alias of itself"]),
    throw({error, Es, Line});

add_alias(DictName, Name, Alias, Line) ->
    ?PRINT("add ~w: ~w for ~w~n", [DictName, Alias, Name]),
    case is_key(Alias, DictName) of
        false -> ok;
        true ->
            Es = lists:concat(["duplicate alias ", Alias]),
            throw({error, Es, Line})
    end,
    case is_key(Name, DictName) of
        false -> ok;
        true ->
            Es_1 = lists:concat([
                "alias definition for previously defined alias ", Name]),
            throw({error, Es_1, Line})
    end,
    store(Alias, Name, DictName).


unalias(DictName, X) ->
    case get(DictName) of
        'undefined' -> X;  % there is no dictionary and aliases
        Dict ->
            case dict:find(X, Dict) of
                'error' -> X; % no alias; return the original name
                {ok, Name} ->
                    ?PRINT("unalias ~w: ~w to ~w~n", [DictName, X, Name]),
                    Name
            end
    end.


rewrite(X = {attribute,_LINE,file,{File,_Line}}) ->
    set_file(File), [X];

rewrite(X = {attribute,LINE,record,{Name,_Fields}}) ->
    add_record_name(Name, LINE),
    [X];

rewrite({attribute,LINE,record_alias,{Alias, RecordName}})
        when is_atom(RecordName), is_atom(Alias) ->
    add_record_alias(RecordName, Alias, LINE),
    [];

% XXX: prohibit definition of aliases in .hrl files?
rewrite({attribute,LINE,record_alias,X}) ->
    %Es = lists:flatten(io_lib:format(
    %      "invalid 'record_alias' specification ~w at line ~w", [X, LINE])),
    %throw({error, Es});
    Es = ["invalid 'record_alias' specification", X],
    throw({error, Es, LINE});

rewrite({attribute,LINE,module_alias,{Alias, ModuleName}})
        when is_atom(ModuleName), is_atom(Alias) ->
    add_module_alias(ModuleName, Alias, LINE),
    [];

rewrite({attribute,LINE,module_alias,X}) ->
    Es = ["invalid 'module_alias' specification", X],
    throw({error, Es, LINE});

rewrite({attribute,LINE,dict_alias,X}) when is_atom(X) ->
    set_dict_alias(X, LINE),
    [];

rewrite({attribute,LINE,dict_alias,X}) ->
    Es = ["invalid 'dict_alias' specification", X],
    throw({error, Es, LINE});

rewrite({function,LINE,Name,Arity,L}) ->
    X = {function,LINE,Name,Arity, clause_list(L, init_state())},
    [X];

rewrite(X) -> [X].


%
% Rewriting the function form
%

-type context() :: body | pattern | guard.

-record(state, {
    context :: context(),
    dict_alias :: atom() % special handling of 'dict' records is enabled
}).


init_state() ->
    #state{
        context = 'undefined',
        dict_alias = get_dict_alias()
    }.


-define(LIST_MAPPER(M, F),
    M(L,S) -> lists:map(fun (X) -> F(X, S) end, L)).


?LIST_MAPPER(clause_list, clause).
?LIST_MAPPER(expr_list, expr).
?LIST_MAPPER(pattern_list, pattern). % pattern_sequence
?LIST_MAPPER(guard_list, guard). % guard sequence
?LIST_MAPPER(field_list, field).
?LIST_MAPPER(bin_list, bin).
?LIST_MAPPER(gen_list, gen).


-define(expr(X), expr(X, S)).
-define(body(X), body(X, S)).
-define(pattern(X), pattern(X, S)).
-define(expr_list(X), expr_list(X, S)).
-define(clause_list(X), clause_list(X, S)).
-define(field_list(X), field_list(X, S)).
-define(gen_list(X), gen_list(X, S)).
-define(bin_list(X), bin_list(X, S)).


-define(is_context(Context), S#state.context == Context).
-define(is_dict_alias(Name), S#state.dict_alias == Name).


clause(_C = {clause,LINE,Ps,Gs,B}, S) ->
    %?PRINT("clause: ~p~n", [_C]),
    {clause,LINE, pattern_list(Ps,S), guard_list(Gs,S), ?body(B)}.


body(X, S) ->
    expr_list(X, S#state{context = body}).

guard(X, S) ->
    expr_list(X, S#state{context = guard}).


pattern({match,LINE,P_1,P_2}, S) ->
    {match,LINE,?pattern(P_1),?pattern(P_2)};

pattern(X, S) ->
    expr(X, S#state{context = pattern}).


% make dict:store(K1, V1, dict:store(K2, V2, dict:store(K3, V3, ...)))
make_dict_store(InitDict, L) ->
    make_dict_store_1(InitDict, lists:reverse(L)).

make_dict_store_1(InitDict, []) -> InitDict;
make_dict_store_1(InitDict, [H|T]) ->
    {record_field,LINE,Key,Value} = H,
    Args = [Key, Value, make_dict_store_1(InitDict, T)],
    Res = make_call(LINE, 'dict', 'store', Args),
    %?PRINT("make_dict_store_1: ~p~n", [Res]),
    Res.


make_call(LINE, ModName, FunName, Args) ->
    Mod = {'atom',LINE,ModName},
    Fun = {'atom',LINE,FunName},
    Res = {call, LINE, {remote,LINE,Mod,Fun}, Args},
    %?PRINT("make_call: ~p~n", [Res]),
    Res.


%
% common function for traversing expressions, patterns and guard elements
%
expr({record,LINE,Name,L}, S) when ?is_dict_alias(Name), ?is_context(body) ->
    % convert #dict{...} to dict:store(Key, Value, dict:store(...))
    InitDict = make_call(LINE, 'dict', 'new', []),
    make_dict_store(InitDict, ?field_list(L));

expr({record,_LINE,E,Name,L}, S) when ?is_dict_alias(Name), ?is_context(body) ->
    % convert D#dict{...} to dict:store(Key, Value, dict:store(..., D))
    make_dict_store(_InitDict = ?expr(E), ?field_list(L));

expr({record_index,LINE,Name,_F}, S)
        when ?is_dict_alias(Name), ?is_context(body) ->
    Es = "field indexes can not be used with dict_alias",
    throw({error, Es, LINE});

expr({record_field,LINE,E,Name,F}, S)
        when ?is_dict_alias(Name), ?is_context(body) ->
    % convert X#dict.foo to dict:fetch(foo, X)
    make_call(LINE, 'dict', 'fetch', [F, ?expr(E)]);

% NOTE: highly experimental: reusing Mensia field access syntax for accessing
% dict members
% Original use: If E is E_0.Field, a Mnesia record access inside a query.
expr({record_field,LINE,E,F}, S)
        when S#state.dict_alias == '', ?is_context(body),
             not (is_tuple(E) % prohibit using ".foo" without leading expression
                  andalso element(1, E) == 'atom'
                  andalso element(3, E) == '') ->
    % convert X.foo to dict:fetch(foo, X)
    %?PRINT("record_field: ~p~n", [E]),
    make_call(LINE, 'dict', 'fetch', [F, ?expr(E)]);

%
% end of special handling of 'dict' records
%
expr({record,LINE,Name,L}, S) ->
    {record,LINE,unalias_record(Name),?field_list(L)};

expr({record,LINE,E,Name,L}, S) ->
    {record,LINE,?expr(E),unalias_record(Name),?field_list(L)};

expr({record_index,LINE,Name,F}, _S) ->
    {record_index,LINE,unalias_record(Name),F};

expr({record_field,LINE,E,Name,F}, S) ->
    {record_field,LINE,?expr(E),unalias_record(Name),F};

expr({match,LINE,P,E_0}, S) ->
    {match,LINE,?pattern(P),?expr(E_0)};

expr({tuple,LINE,L}, S) ->
    {tuple,LINE,?expr_list(L)};

expr({cons,LINE,P_h,P_t}, S) ->
    {cons,LINE,?expr(P_h),?expr(P_t)};

expr({bin,LINE, L}, S) ->
    {bin,LINE, ?bin_list(L)};

expr({op,LINE,Op,P_1,P_2}, S) ->
    {op,LINE,Op,?expr(P_1),?expr(P_2)};

expr({op,LINE,Op,P}, S) ->
    {op,LINE,Op,?expr(P)};

expr({'catch',LINE,E}, S) ->
    {'catch',LINE,?expr(E)};

expr({call, LINE, {remote,LINE_1,M,F}, L}, S) ->
    M1 = unalias_module_atom(M),
    {call, LINE, {remote,LINE_1,?expr(M1),?expr(F)}, ?expr_list(L)};

expr({call,LINE,F,L}, S) ->
    {call,LINE,?expr(F),?expr_list(L)};

% list comprehension
expr({lc,LINE,E,L}, S) ->
    {lc,LINE,?expr(E),?gen_list(L)};

% binary comprehension
expr({bc,LINE,E,L}, S) ->
    {bc,LINE,?expr(E),?gen_list(L)};

% If E is query [E_0 || W_1, ..., W_k] end, where each W_i is a generator or a filter
expr({'query',LINE,{lc,LINE,E_0,L}}, S) ->
    {'query',LINE,{lc,LINE,?expr(E_0),?gen_list(L)}};

% begin .. end
expr({block,LINE,B}, S) ->
    {block,LINE,?body(B)};

expr({'if',LINE,L}, S) ->
    {'if',LINE,?clause_list(L)};

expr({'case',LINE,E_0,L}, S) ->
    {'case',LINE,?expr(E_0),?clause_list(L)};

% try ... of ... catch ... after ... end
expr(_X = {'try',LINE,B,Clauses,CatchClauses,After}, S) ->
    %?PRINT("try: ~p~n", [_X]),
    {'try',LINE,?body(B),?clause_list(Clauses),?clause_list(CatchClauses),?body(After)};

% receive ... end
expr({'receive',LINE,L,E_0,B_t}, S) ->
    {'receive',LINE,?clause_list(L),?expr(E_0),?body(B_t)};

% receive ... after ... end
expr({'receive',LINE,L}, S) ->
    {'receive',LINE,?clause_list(L)};

expr(X = {'fun',_LINE,{function,_Name,_Arity}}, _S) ->
    X;
expr({'fun',LINE,{function,Module,Name,Arity}}, _S) ->
    {'fun',LINE,{function,unalias_module(Module),Name,Arity}};

expr({'fun',LINE,{clauses,L}}, S) ->
    {'fun',LINE,{clauses,?clause_list(L)}};

% If E is E_0.Field, a Mnesia record access inside a query
expr({record_field,LINE,E_0,Field}, S) ->
    {record_field,LINE,?expr(E_0),Field};

expr(Atomic = {A,_LINE,_Value}, _S)
        when A == 'integer'; A == 'float'; A == 'string';
             A == 'atom'; A == 'char' ->
    Atomic;

expr(Var = {var,_LINE,_A}, _S) -> % variable or variable pattern
    Var;

expr(X = {nil,_LINE}, _S) -> X;

% If C is a catch clause X : P when Gs -> B where X is an atomic literal or a
% variable pattern, P is a pattern, Gs is a guard sequence and B is a body, then
% Rep(C) = {clause,LINE,[Rep({X,P,_})],Rep(Gs),Rep(B)}.
expr(_Cc = {Class,P,'_'}, S) ->
    %?PRINT("catch clause: ~p~n", [_Cc]),
    {?expr(Class),?expr(P),'_'};

expr(X, _S) ->
    throw({missing_rule, X}).
    %X.


field({record_field,LINE,F,E}, S) ->
    {record_field,LINE,F,?expr(E)}.


bin({bin_element,LINE,P,Size,TSL}, S) ->
    {bin_element,LINE,?expr(P),Size,TSL}.


% When W is a generator or a filter (in the body of a list or binary comprehension), then:
%
%    * If W is a generator P <- E, where P is a pattern and E is an ?expression, then Rep(W) = {generate,LINE,Rep(P),Rep(E)}.
%    * If W is a generator P <= E, where P is a pattern and E is an ?expression, then Rep(W) = {b_generate,LINE,Rep(P),Rep(E)}.
%    * If W is a filter E, which is an ?expression, then Rep(W) = Rep(E).
gen({generate,LINE,P,E}, S) ->
    {generate,LINE,?pattern(P),?expr(E)};

gen({b_generate,LINE,P,E}, S) ->
    {b_generate,LINE,?pattern(P),?expr(E)};

gen(X, S) -> ?expr(X).

