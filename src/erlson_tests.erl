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
    _ = #{},

    % associating foo with 1
    D = #{foo = 1},
    1 = D.foo,
    1 = #{foo = 1}.foo,

    % setting foo to foo + 1 and baz to 100
    D1 = D#{foo = D.foo + 1}#{baz = 100},
    2 = D1.foo,
    100 = D1.baz,

    % accessing undefined field
    {'EXIT', {badarg, _}} = (catch D.bar),

    % several elements
    D2 = D#{bar = 1, fum = 10, obj = D},
    1 = D2.bar,
    10 = D2.fum,

    % accessing field of a dict included in another dict
    1 = D2.obj.foo,
    ok.


grammar_test() ->
    dict_grammar(#{foo = 1}),
    foo().


dict_grammar(D) -> D.foo, D. foo() -> ok.

