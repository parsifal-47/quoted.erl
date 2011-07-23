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
-module(quoted).
-on_load(load_nif/0).

%% exported functions
-export([to_url/1,
         from_url/1]).

%% internal functions
-export([to_url_/1, from_url_/1]).
-export([is_native/0]).



-type data()    :: [byte()] | binary().

load_nif() ->
    erlang:load_nif(nif_path(), 0).

-spec nif_path() -> string().
nif_path() ->
    So = "quoted",
    case code:priv_dir(quoted) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                File when not is_list(File) -> filename:join("../priv", So);
                File -> filename:join([filename:dirname(File),"../priv", So])
            end;
         Dir ->
            filename:join(Dir, So)
    end.

-spec is_native() -> boolean().
is_native() -> false.


%% @doc
%% @end
-spec to_url(data()) -> data().
to_url(Data) ->
    to_url_(Data).

to_url_(Str) when is_list(Str) ->
    quote_list_to_list(Str);
to_url_(Bin) when is_binary(Bin) ->
    quote_bin_to_bin(Bin).


%% @doc
%% @end
-spec from_url(data()) -> data().
from_url(Data) ->
    from_url_(Data).

from_url_(Str) when is_list(Str) ->
    unquote_list_to_list(Str);
from_url_(Bin) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin).


-spec unquote_list_to_list([byte()]) -> [byte()].
unquote_list_to_list([$%|HT]) ->
    [HH,HL|T] = HT,
    H = unhex(HH),
    L = unhex(HL),
    C = tobyte(H, L),
    [C|unquote_list_to_list(T)];
unquote_list_to_list([AC|T]) when is_integer(AC) ->
    C = from_url_alias(AC),
    [C|unquote_list_to_list(T)];
unquote_list_to_list([]) ->
    [].


-spec unquote_bin_to_bin(binary()) -> binary().
unquote_bin_to_bin(Bin) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin, <<>>).

-spec unquote_bin_to_bin(binary(), binary()) -> binary().
unquote_bin_to_bin(<<$%,HT/binary>>, Acc) ->
    <<HH,HL,T/binary>> = HT,
    H = unhex(HH),
    L = unhex(HL),
    C = tobyte(H, L),
    unquote_bin_to_bin(T, <<Acc/binary, C>>);
unquote_bin_to_bin(<<AC, T/binary>>, Acc) ->
    C = from_url_alias(AC),
    unquote_bin_to_bin(T, <<Acc/binary, C>>);
unquote_bin_to_bin(<<>>, Acc) ->
    Acc.


-spec quote_list_to_list([byte()]) -> [byte()].
quote_list_to_list([AC|T]) ->
    case is_url_safe(AC) of
        true ->
            [AC|quote_list_to_list(T)];
        false ->
            H = tohex(highbits(AC)),
            L = tohex(lowbits(AC)),
            [$%,H,L|quote_list_to_list(T)]
    end;
quote_list_to_list([]) ->
    [].


-spec quote_bin_to_bin(binary()) -> binary().
quote_bin_to_bin(Bin) when is_binary(Bin) ->
    quote_bin_to_bin(Bin, <<>>).

-spec quote_bin_to_bin(binary(), binary()) -> binary().
quote_bin_to_bin(<<AC, T/binary>>, Acc) ->
    case is_url_safe(AC) of
        true ->
            quote_bin_to_bin(T, <<Acc/binary, AC>>);
        false ->
            H = tohex(highbits(AC)),
            L = tohex(lowbits(AC)),
            quote_bin_to_bin(T, <<Acc/binary, $%, H, L>>)
    end;
quote_bin_to_bin(<<>>, Acc) ->
    Acc.


-spec highbits(byte()) -> byte().
highbits(I) -> I bsr 4.

-spec lowbits(byte()) -> byte().
lowbits(I)  -> I band 16#0F.

-spec tobyte(byte(), byte()) -> byte().
tobyte(H, L) -> L bor (H bsl 4).

-spec tohex(byte()) -> byte().
tohex(C) ->
    case C of
        0  -> $0;
        1  -> $1;
        2  -> $2;
        3  -> $3;
        4  -> $4;
        5  -> $5;
        6  -> $6;
        7  -> $7;
        8  -> $8;
        9  -> $9;
        10 -> $a;
        11 -> $b;
        12 -> $c;
        13 -> $d;
        14 -> $e;
        15 -> $f
    end.

-spec unhex(byte()) -> byte().
unhex(C) ->
    case C of
        $0 -> 0;
        $1 -> 1;
        $2 -> 2;
        $3 -> 3;
        $4 -> 4;
        $5 -> 5;
        $6 -> 6;
        $7 -> 7;
        $8 -> 8;
        $9 -> 9;
        $A -> 10; $a -> 10;
        $B -> 11; $b -> 11;
        $C -> 12; $c -> 12;
        $D -> 13; $d -> 13;
        $E -> 14; $e -> 14;
        $F -> 15; $f -> 15
    end.


-spec is_url_safe(byte()) -> boolean().
is_url_safe(C) ->
    case C of
        %% Lowercase
        $a -> true; $b -> true; $c -> true; $d -> true; $e -> true;
        $f -> true; $g -> true; $h -> true; $i -> true; $j -> true;
        $k -> true; $l -> true; $m -> true; $n -> true; $o -> true;
        $p -> true; $q -> true; $r -> true; $s -> true; $t -> true;
        $u -> true; $v -> true; $w -> true; $x -> true; $y -> true;
        $z -> true;
        %% Uppercase
        $A -> true; $B -> true; $C -> true; $D -> true; $E -> true;
        $F -> true; $G -> true; $H -> true; $I -> true; $J -> true;
        $K -> true; $L -> true; $M -> true; $N -> true; $O -> true;
        $P -> true; $Q -> true; $R -> true; $S -> true; $T -> true;
        $U -> true; $V -> true; $W -> true; $X -> true; $Y -> true;
        $Z -> true;
        %% Numbers
        $0 -> true; $1 -> true; $2 -> true; $3 -> true; $4 -> true;
        $5 -> true; $6 -> true; $7 -> true; $8 -> true; $9 -> true;
        %% Exceptions
        $. -> true; $- -> true; $~ -> true; $_ -> true;
        %% Unsafe
        _ -> false
    end.


-spec from_url_alias(byte()) -> byte().
from_url_alias(C) ->
    case C of
        $+ -> $\ ;
        _  -> C
    end.
