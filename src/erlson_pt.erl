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

-module(erlson_pt).

-export([parse_transform/2]).

-export([make_dict_new/2, make_dict_store/2, make_dict_fetch/3]).
-export([is_valid_dict_path/1]).


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
    ?PRINT("PARSE TRANSFORM~n", []),
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
    put('__erlson_file__', File).

get_file() ->
    get('__erlson_file__').


rewrite(X = {attribute,_LINE,file,{File,_Line}}) ->
    set_file(File), [X];

rewrite({function,LINE,Name,Arity,L}) ->
    X = {function,LINE,Name,Arity, clause_list(L, init_state())},
    [X];

rewrite(X) -> [X].


%
% Rewriting the function form
%

-type context() :: body | pattern | guard.

-record(state, {
    context :: context()
}).


init_state() ->
    #state{
        context = 'undefined'
    }.


-define(LIST_MAPPER(M, F),
    M(L,S) -> lists:map(fun (X) -> F(X, S) end, L)).


?LIST_MAPPER(clause_list, clause).
?LIST_MAPPER(expr_list, expr).
?LIST_MAPPER(pattern_list, pattern). % pattern_sequence
?LIST_MAPPER(guard_list, guard). % guard sequence
?LIST_MAPPER(field_list, field).
?LIST_MAPPER(map_field_list, map_field).
?LIST_MAPPER(bin_list, bin).
?LIST_MAPPER(gen_list, gen).


-define(expr(X), expr(X, S)).
-define(body(X), body(X, S)).
-define(pattern(X), pattern(X, S)).
-define(expr_list(X), expr_list(X, S)).
-define(clause_list(X), clause_list(X, S)).
-define(field_list(X), field_list(X, S)).
-define(map_field_list(X), map_field_list(X, S)).
-define(gen_list(X), gen_list(X, S)).
-define(bin_list(X), bin_list(X, S)).


-define(is_context(Context), S#state.context == Context).


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


% make erlson:store(K1, V1, erlson:store(K2, V2, erlson:store(K3, V3, ...)))
make_dict_store(InitDict, L) ->
    make_dict_store_1(InitDict, lists:reverse(L)).

make_dict_store_1(InitDict, []) -> InitDict;
make_dict_store_1(InitDict, [H|T]) ->
    {record_field,LINE,Name,Value} = H,
    NamePath =
        case Name of
            [X] -> X; % path contains a single atom, like in normal records
            L -> % path contains several atoms separated by '.'
                make_list(L)
        end,
    Args = [NamePath, Value, make_dict_store_1(InitDict, T)],
    Res = make_call(LINE, 'erlson', 'store', Args),
    %?PRINT("make_dict_store_1: ~p~n", [Res]),
    Res.


make_dict_new(LINE, L) ->
    InitDict = {nil, LINE},
    make_dict_store(InitDict, L).


make_dict_fetch(LINE, FieldName, Dict) ->
    make_call(LINE, 'erlson', 'fetch', [FieldName, Dict]).


make_call(LINE, ModName, FunName, Args) ->
    Mod = {'atom',LINE,ModName},
    Fun = {'atom',LINE,FunName},
    Res = {call, LINE, {remote,LINE,Mod,Fun}, Args},
    %?PRINT("make_call: ~p~n", [Res]),
    Res.


make_list(L) ->
    make_list(L, 0).

make_list([H|T], _LINE) ->
    LINE = element(2, H),
    {cons,LINE,H,make_list(T, LINE)};
make_list([], LINE) -> {nil, LINE}.


% firt two clauses: 'record_field' form a sequence of dot-separated atoms,
% for example: foo.bar.baz , foo, .foo
is_valid_dict_path(E) when is_atom(E) -> false;
is_valid_dict_path({'atom', _, _}) -> false;

% make sure we don't interfere with Mnesia/QLC by treating '_' as a valid
% variable
% NOTE: this syntax was deprecated in R16A or maybe even in earlier releases
is_valid_dict_path({'var', _, '_'}) -> false;

% got another path element, calling recursively:
is_valid_dict_path({record_field,_LINE,E,_F}) -> is_valid_dict_path(E); % only used pre- R16A
is_valid_dict_path({record_field,_LINE,E,'',_F}) -> is_valid_dict_path(E);

% everything else should be OK
is_valid_dict_path(_) -> true.


%
% common function for traversing expressions, patterns and guard elements
%

% this node was returned by the customized Erlang parser
expr({record,LINE,'',L}, S) -> %when ?is_context(body) ->
    % convert #{...} to erlson:store(Key, Value, erlson:store(..., []))
    make_dict_new(LINE, ?field_list(L));

% this node was returned by the customized Erlang parser
expr({record,_LINE,E,'',L}, S) -> %when ?is_context(body) ->
    % convert D#{...} to erlson:store(Key, Value, erlson:store(..., D))
    make_dict_store(_InitDict = ?expr(E), ?field_list(L));

% this node was returned by the customized Erlang parser
expr({record_field,LINE,E,'',F}, S) -> %when ?is_context(body) ->
    % convert X.foo to erlson:fetch(foo, X)
    make_dict_fetch(LINE, F, ?expr(E));

% NOTE: this brach is used only for Erlang releases < R16A; starting from Erlang
% R16A, this AST element is no longer used. The above clause is used instead.
%
% NOTE: reusing Mensia (qlc) field access syntax for accessing dict members
% Original use: If E is E_0.Field, a Mnesia record access inside a query.
expr(X = {record_field,LINE,E,F}, S) %when ?is_context(body)
    when not is_atom(E),
         not (is_tuple(E) andalso element(1, E) == 'atom') ->
    case is_valid_dict_path(E) of
        true ->
            % convert X.foo to erlson:fetch(foo, X)
            %?PRINT("record_field: ~p~n", [E]),
            make_dict_fetch(LINE, F, ?expr(E));
        false ->
            % NOTE: there's no need to interpret (recurse into) invalid dict
            % paths because they can't contain nodes that might have other
            % Erlson expression
            X
    end;

%
% end of special handling of 'dict' records
%
expr({record,LINE,Name,L}, S) ->
    {record,LINE,Name,?field_list(L)};

expr({record,LINE,E,Name,L}, S) ->
    {record,LINE,?expr(E),Name,?field_list(L)};

expr({record_index,LINE,Name,F}, _S) ->
    {record_index,LINE,Name,F};

expr({record_field,LINE,E,Name,F}, S) ->
    {record_field,LINE,?expr(E),Name,F};

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

expr({map,LINE,L}, S) ->
    {map,LINE,?map_field_list(L)};

expr({map,LINE,E,L}, S) ->
    {map,LINE,?expr(E),?map_field_list(L)};

expr({'catch',LINE,E}, S) ->
    {'catch',LINE,?expr(E)};

expr({call, LINE, {remote,LINE_1,M,F}, L}, S) ->
    {call, LINE, {remote,LINE_1,?expr(M),?expr(F)}, ?expr_list(L)};

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
expr({'fun',LINE,{function,Module,Name,Arity}}, S) ->
    {'fun',LINE,{function,?expr(Module),?expr(Name),?expr(Arity)}};

expr({'fun',LINE,{clauses,L}}, S) ->
    {'fun',LINE,{clauses,?clause_list(L)}};

expr({named_fun,LINE,Name,L}, S) ->
    {named_fun,LINE,Name,?clause_list(L)};

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


map_field({map_field_assoc,LINE,K,V}, S) ->
    {map_field_assoc,LINE,?expr(K),?expr(V)};

map_field({map_field_exact,LINE,K,V}, S) ->
    {map_field_exact,LINE,?expr(K),?expr(V)}.


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

