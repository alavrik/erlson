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
-export([fetch/2, store/3]).


% dictionary represented as an ordered list of name-value pairs
-type orddict() :: [ {name(), value()} ].
-type name() :: atom() | binary().
-type value() :: name().


-spec fetch/2 :: (
    Name :: atom(),
    Dict :: orddict() ) -> value().

fetch(Name, Dict) ->
    try
        fetch_val(Name, Dict)
    catch
        'erlson_not_found' ->
            erlang:error('erlson_not_found', [Name, Dict])
    end.


fetch_val(Name, [{N, _V} | T]) when Name > N ->
    fetch_val(Name, T);
fetch_val(Name, [{N, V} | _T]) when Name =:= N -> V;
fetch_val(_Name, _) ->
    throw('erlson_not_found').


-spec store/3 :: (
    Name :: atom(),
    Value :: any(),
    Dict :: orddict() ) -> orddict().

store(Name, Value, Dict) ->
    orddict:store(Name, Value, Dict).

