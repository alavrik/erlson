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

-module(erlson_shell_test).
%-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


compile_test() ->
    % load a different version of erl_parse.beam that contains in-place Erlson
    % transformations
    erlson:init(),
    ErlParsePath = code:which(erl_parse),

    %?debugFmt("AAAAAAAAAAA: erl_parse.beam: ~s~n", [ErlParsePath]),
    ?assert(string:str(ErlParsePath, "erl_parse_shell.beam") =/= 0),

    % now, compile a test file using a different parser
    Res = compile:file("erlson_tests.erl"),

    %?debugFmt("AAAAAAAAAAA: erlson_shell compilation: ~p~n", [Res]),
    ?assertEqual(element(1, Res), ok),

    ok.

