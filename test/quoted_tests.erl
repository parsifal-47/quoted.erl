%%
%%   Copyright 2011 Magnus Klaar
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(quoted_tests).
-include_lib("eunit/include/eunit.hrl").
-ifdef(PROPER).
-undef(LET).
-include_lib("proper/include/proper.hrl").
-endif.
-define(q, quoted).

% Library behaviour:
%   " "<->"%20"
%   " "<--"+"
%
% Simple and more according to standard, but still supports the popular +
% encoding.
space_char_test_() ->
    % " "<--"%20"
    [?_assertEqual(" ", ?q:from_url("%20")),
     ?_assertEqual(<<" ">>, ?q:from_url(<<"%20">>)),
    % " "<--"+"
     ?_assertEqual(" ", ?q:from_url("+")),
     ?_assertEqual(<<" ">>, ?q:from_url(<<"+">>)),
    % " "-->"%20" 
     ?_assertEqual("%20", ?q:to_url(" ")),
     ?_assertEqual(<<"%20">>, ?q:to_url(<<" ">>))].

-ifdef(PROPER).

bstring() ->
    list(byte()).

binstring() ->
    binary().

list_inv_prop() ->
    ?FORALL(Input, bstring(),
    begin
        Quoted = ?q:to_url(Input),
        Unquoted = ?q:from_url(Quoted),
        Unquoted == Input
    end).

bin_inv_prop() ->
    ?FORALL(Input, binstring(),
    begin
        Quoted = ?q:to_url(Input),
        Unquoted = ?q:from_url(Quoted),
        Unquoted == Input
    end).

list_inv_test() -> ?assert(proper:quickcheck(list_inv_prop())).
bin_inv_test() -> ?assert(proper:quickcheck(bin_inv_prop())).

-endif.
