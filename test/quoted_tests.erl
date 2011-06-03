-module(quoted_tests).
-include_lib("eunit/include/eunit.hrl").
-ifdef(PROPER).
-undef(LET).
-include_lib("proper/include/proper.hrl").
-endif.
-define(q, quoted).

space_char_test_() ->
    [?_assertEqual(" ", ?q:to_list("+")),
     ?_assertEqual(" ", ?q:to_list(<<"+">>)),
     ?_assertEqual(<<" ">>, ?q:from_url("+")),
     ?_assertEqual(<<" ">>, ?q:from_url(<<"+">>)),
     ?_assertEqual("+", ?q:to_url(" ")),
     ?_assertEqual("+", ?q:to_url(<<" ">>)),
     ?_assertEqual(<<"+">>, ?q:to_url(" ")),
     ?_assertEqual(<<"+">>, ?q:to_url(" "))].

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

list_via_bin_inv_prop() ->
    ?FORALL(Input, bstring(),
    begin
        Quoted = ?q:to_url(Input),
        Unquoted = ?q:to_list(Quoted),
        Unquoted == Input
    end).

bin_via_list_inv_prop() ->
    ?FORALL(Input, binstring(),
    begin
        Quoted = ?q:to_url(Input),
        Unquoted = ?q:from_url(Quoted),
        Unquoted == Input
    end).


list_inv_test() -> ?assert(proper:quickcheck(list_inv_prop())).
bin_inv_test() -> ?assert(proper:quickcheck(bin_inv_prop())).
list_via_bin_inv_test() -> ?assert(proper:quickcheck(list_via_bin_inv_prop())).
bin_via_list_inv_test() -> ?assert(proper:quickcheck(bin_via_list_inv_prop())).

-endif.
