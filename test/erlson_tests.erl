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

-module(erlson_tests).
%-compile(export_all).

-include("../include/erlson.hrl").
-include_lib("eunit/include/eunit.hrl").


basic_test() ->
    % creating an empty dictionary
    _ = #[],

    % associating foo with 1
    D = #[foo = 1],
    1 = D.foo,
    1 = #[foo = 1].foo,

    % setting foo to foo + 1 and baz to 100
    D1 = D#[foo = D.foo + 1]#[baz = 100],
    2 = D1.foo,
    100 = D1.baz,

    % accessing undefined field
    {'EXIT', {'erlson_not_found', _}} = (catch D.bar),

    % several elements
    D2 = D#[bar = 1, fum = 10, obj = D],
    1 = D2.bar,
    10 = D2.fum,

    % accessing field of a dict included in another dict
    1 = D2.obj.foo,
    ok.


extended_test() ->
    % setting a new dict with a field set to 'true'
    D = #[foo],
    true = D.foo,

    D1 = D#[foo = false, bar = 10],
    10 = D1.bar,
    false = D1.foo,

    % creating a nested dict
    D2 = #[foo = #[]],

    % now, setting a field in the nested dict
    D3 = D2#[foo.bar = 1],
    1 = D3.foo.bar,

    % setting several fields in the nested dicts
    D4 = #[
        foo = #[],
        foo.bar = 1,
        foo.baz = #[],
        foo.baz.fum % = true
    ],
    1 = D4.foo.bar,
    true = D4.foo.baz.fum,

    ok.


grammar_test() ->
    dict_grammar(#[foo = 1]),
    foo(),
    dict_grammar1(),
    foo1(),
    dict_grammar2(),
    foo2(),
    dict_grammar3(),
    foo3(),
    ok.

dict_grammar(D) -> D.foo, D. foo() -> ok.

dict_grammar1() -> #[x = 1]. foo1() -> ok.

dict_grammar2() -> #[x = 1].x. foo2() -> ok.

dict_grammar3() -> D = #[x = 1], D.x. foo3() -> ok.


json_test() ->
    case code:which(mochijson2) of
        'non_existing' ->
            ?debugMsg(
                "can't test erlson:to/from_json, because mochijson2 module is not found"
            );
        _ ->
            json1(),
            json_empty(),
            json_empty_list(),
            json_null(),
            ok
    end.


json1() ->
    Json =
    "  {
          \"Image\": {
              \"Width\":  800,
              \"Height\": 600,
              \"Title\":  \"View from 15th Floor\",
              \"Thumbnail\": {
                  \"Url\":    \"http://www.example.com/image/481989943\",
                  \"Height\": 125,
                  \"Width\":  \"100\"
              },
              \"IDs\": [116, 943, 234, 38793]
            },
          \"address\": {
             \"precision\": \"zip\",
             \"Latitude\":  37.7668,
             \"Longitude\": -122.3959,
             \"Address\":   \"\",
             \"City\":      \"SAN FRANCISCO\",
             \"State\":     \"CA\",
             \"Zip\":       \"94107\",
             \"Country\":   \"US\"
          }
       }
    ",
    D = erlson:from_json(Json),

    800 = D.'Image'.'Width',
    125 = D.'Image'.'Thumbnail'.'Height',
    <<"100">> = D.'Image'.'Thumbnail'.'Width',
    [116, 943, 234, 38793] = D.'Image'.'IDs',

    _ = D.address,
    <<"CA">> = D.address.'State',
    true = is_float(D.address.'Latitude'),

    ok.


json_empty() ->
    D = #[],
    J = <<"{}">> = iolist_to_binary(erlson:to_json(D)),

    ?assert(erlson:from_json(J) =:= D),
    ok.


json_empty_list() ->
    D = #[foo = []],
    J = <<"{\"foo\":[]}">> = iolist_to_binary(erlson:to_json(D)),

    ?assert(erlson:from_json(J) =:= D),
    ok.


json_null() ->
    D = #[foo = 'undefined'],
    J = <<"{\"foo\":null}">> = iolist_to_binary(erlson:to_json(D)),

    ?assert(erlson:from_json(J) =:= D),
    ok.


proplist_test() ->
    ?assertEqual(#[], erlson:from_proplist([])),
    ?assertEqual(#[], erlson:from_nested_proplist([])),

    ?assertEqual(#[foo], erlson:from_proplist([foo])),
    ?assertEqual(#[foo], erlson:from_nested_proplist([foo])),

    ?assertEqual(#[foo], erlson:from_proplist([{foo, true}])),
    ?assertEqual(#[foo], erlson:from_nested_proplist([{foo, true}])),

    L =
         [{description, "Erlang Simple Object Notation"},
          {vsn, git},
          {modules, []},
          {applications, [kernel, stdlib]},
          {env, []}],
    D =
         #[description = "Erlang Simple Object Notation",
           vsn = git,
           modules = [],
           applications = [kernel, stdlib],
           env = []],

    NestedD =
         #[description = "Erlang Simple Object Notation",
           vsn = git,
           modules = [],
           applications = #[kernel, stdlib],
           env = []],

    ?assertEqual(D, erlson:from_proplist(L)),
    ?assertEqual(D, erlson:from_nested_proplist(L,1)),

    ?assertEqual(NestedD, erlson:from_nested_proplist(L)),
    ?assertEqual(NestedD, erlson:from_nested_proplist(L, 'undefined')),

    ?assertEqual(NestedD, erlson:from_nested_proplist(L,2)),
    ?assertEqual(NestedD, erlson:from_nested_proplist(L,10)),

    ok.

