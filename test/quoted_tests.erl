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

%% Verify that the decoder throws a badarg error if any of the
%% two characters following a percent-character isn't a valid
%% hex-character.
invalid_hex_test_() ->
    [%% Second character after % is invalid
     ?_assertError(badarg, ?q:from_url("%Aj")),
     ?_assertError(badarg, ?q:from_url(<<"%Aj">>)),
     %% First character after % is invalid
     ?_assertError(badarg, ?q:from_url("%jA")),
     ?_assertError(badarg, ?q:from_url(<<"%jA">>)),
     %% Both characters after % are invalid
     ?_assertError(badarg, ?q:from_url("%ij")),
     ?_assertError(badarg, ?q:from_url(<<"%ij">>))].

%% Verify that the decoder throws a badarg error if a percent
%% character is not followed by at least two characters.
insufficient_hex_test_() ->
    [%% No characters after % is invalid
     ?_assertError(badarg, ?q:from_url("%")),
     ?_assertError(badarg, ?q:from_url(<<"%">>)),
     %% One character after % is invalid
     ?_assertError(badarg, ?q:from_url("%A")),
     ?_assertError(badarg, ?q:from_url(<<"%A">>))].

%% The ?_assertError(badarg, ?q:from_url(<<"%A">>)) assertion
%% occasionally failed when running the test suite multiple times.
insufficient_hex_determinstic_test() ->
    Input = fun() -> list_to_binary("%A") end,
    _ = [?assertError(badarg, ?q:from_url(Input())) || _ <- lists:seq(1, 10000)],
    ok.


%% Verify that any binaries allocated in the NIF when
%% decoding invalid input are released by the NIF and
%% freed by the erlang runtime system.
decode_memory_leak_test() ->
    BinMemBefore = erlang:memory(binary),
    {_Pid, MRef} = erlang:spawn_monitor(fun() ->
        %% Use a binary that is 1MB large and unquote that binary
        %% 1024 times, this shoud result in a noticable 1GB leak
        %% if a memory leak is occuring.
        Input = binary:copy(<<"%AI">>, (1024*1024) div 2),
        _ = [catch quoted:from_url(Input) || _ <- lists:seq(1, 1024)],
        erlang:garbage_collect()
    end),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    BinMemAfter = erlang:memory(binary),
    BinMemDiff = BinMemAfter / BinMemBefore,
    %% TODO: less fuzzy assertion?
    ?assert(BinMemDiff < 1.1).

%% Verify that any binaries allocated in the NIF when encoding
%% are released by the NIF an freed by the erlang runtime system.
encode_memory_leak_test() ->
    BinMemBefore = erlang:memory(binary),
    {_Pid, MRef} = erlang:spawn_monitor(fun() ->
        %% Use a binary that is 1MB large and unquote that binary
        %% 1024 times, this shoud result in a noticable 1GB leak
        %% if a memory leak is occuring.
        Input = binary:copy(<<"AA">>, (1024*1024) div 2),
        _ = [catch quoted:to_url(Input) || _ <- lists:seq(1, 1024)],
        erlang:garbage_collect()
    end),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    BinMemAfter = erlang:memory(binary),
    BinMemDiff = BinMemAfter / BinMemBefore,
    %% TODO: less fuzzy assertion?
    ?assert(BinMemDiff < 1.1).




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

bin_oracle_decode_prop() ->
    ?FORALL(Input, binstring(),
        safe_from_url(native, Input) =:= safe_from_url(erlang, Input)).

list_oracle_decode_prop() ->
    ?FORALL(Input, bstring(),
        safe_from_url(native, Input) =:= safe_from_url(erlang, Input)).

bin_oracle_encode_prop() ->
    ?FORALL(Input, binstring(),
        ?q:to_url(Input) =:= ?q:to_url_(Input)).

list_oracle_encode_prop() ->
    ?FORALL(Input, bstring(),
        ?q:to_url(Input) =:= ?q:to_url_(Input)).


list_inv_test() -> ?assert(proper:quickcheck(list_inv_prop())).
bin_inv_test() -> ?assert(proper:quickcheck(bin_inv_prop())).
bin_oracle_decode_test() -> ?assert(proper:quickcheck(bin_oracle_decode_prop())).
list_oracle_decode_test() -> ?assert(proper:quickcheck(list_oracle_decode_prop())).
bin_oracle_encode_test() -> ?assert(proper:quickcheck(bin_oracle_encode_prop())).
list_oracle_encode_test() -> ?assert(proper:quickcheck(list_oracle_encode_prop())).

safe_from_url(Impl, String) ->
    Result = case Impl of
        erlang -> (catch ?q:from_url_(String));
        native -> (catch ?q:from_url(String))
    end,
    case Result of
        {'EXIT', _} -> error;
        Other -> Other
    end.


-endif.
