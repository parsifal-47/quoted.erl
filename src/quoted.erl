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
-export([make/1,
         to_url/1,
         to_url/2,
         from_url/1,
         from_url/2]).

%% internal functions
-export([to_url_/1, from_url_/1, to_url_/2, from_url_/2]).
-export([is_native/0]).



-type data() :: [byte()] | binary().
-type option()
   :: {charcase, lower | upper}
    | {strict, boolean()}
    | {plus, boolean()}.

-record(options, {
    charcase :: lower | upper,
    strict :: boolean(),
    plus :: boolean()}).
-opaque options() :: #options{}.
-export_type([options/0]).

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
-spec make([option()]) -> options().
make(OptionsList) ->
    Default = defaults(),
    Case = case lists:keyfind(charcase, 1, OptionsList) of
        {charcase, lower} -> lower;
        {charcase, upper} -> upper;
        false -> Default#options.charcase;
        _ -> erlang:error(badarg)
    end,
    Strict = case lists:keyfind(strict, 1, OptionsList) of
        {strict, true} -> true;
        {strict, false} -> false;
        false -> Default#options.strict;
        _ -> erlang:error(badarg)
    end,
    Plus = case lists:keyfind(plus, 1, OptionsList) of
        {plus, true} -> true;
        {plus, false} -> false;
        false -> Default#options.plus;
        _ -> erlang:error(badarg)
    end,
    #options{charcase=Case, strict=Strict, plus=Plus}.

%% @private Return the default options.
defaults() ->
    #options{charcase=lower, strict=false, plus=false}.


%% @doc
%% @end
-spec to_url(data()) -> data().
to_url(Data) ->
    to_url_(Data).


to_url_(Data) ->
    to_url_(Data, defaults()).


%% @doc
%% @end
-spec to_url(data(), options()) -> data().
to_url(Data, Options) ->
    to_url_(Data, Options).


to_url_(Str, Options) when is_list(Str) ->
    quote_list_to_list(Str, Options);
to_url_(Bin, Options) when is_binary(Bin) ->
    quote_bin_to_bin(Bin, Options).


%% @doc
%% @end
-spec from_url(data()) -> data().
from_url(Data) ->
    from_url_(Data).


from_url_(Data) ->
    from_url_(Data, defaults()).


%% @doc
%% @end
-spec from_url(data(), options()) -> data().
from_url(Data, Options) ->
    from_url_(Data, Options).


from_url_(Str, Options) when is_list(Str) ->
    unquote_list_to_list(Str, Options);
from_url_(Bin, Options) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin, Options).


-spec unquote_list_to_list([byte()], options()) -> [byte()].
unquote_list_to_list([$%|HT], _Options) ->
    [HH,HL|T] = HT,
    H = unhex(HH),
    L = unhex(HL),
    C = tobyte(H, L),
    [C|unquote_list_to_list(T, _Options)];
unquote_list_to_list([AC|T], _Options) when is_integer(AC) ->
    C = from_url_alias(AC),
    [C|unquote_list_to_list(T, _Options)];
unquote_list_to_list([], _Options) ->
    [].


-spec unquote_bin_to_bin(binary(), options()) -> binary().
unquote_bin_to_bin(Bin, Options) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin, Options, <<>>).

-spec unquote_bin_to_bin(binary(), options(), binary()) -> binary().
unquote_bin_to_bin(<<$%,HT/binary>>, _Options, Acc) ->
    <<HH,HL,T/binary>> = HT,
    H = unhex(HH),
    L = unhex(HL),
    C = tobyte(H, L),
    unquote_bin_to_bin(T, _Options, <<Acc/binary, C>>);
unquote_bin_to_bin(<<AC, T/binary>>, _Options, Acc) ->
    C = from_url_alias(AC),
    unquote_bin_to_bin(T, _Options, <<Acc/binary, C>>);
unquote_bin_to_bin(<<>>, _Options, Acc) ->
    Acc.


-spec quote_list_to_list([byte()], options()) -> [byte()].
quote_list_to_list([AC|T], _Options) ->
    case is_url_safe(AC) of
        true ->
            [AC|quote_list_to_list(T, _Options)];
        false ->
            H = tohex(highbits(AC)),
            L = tohex(lowbits(AC)),
            [$%,H,L|quote_list_to_list(T, _Options)]
    end;
quote_list_to_list([], _Options) ->
    [].


-spec quote_bin_to_bin(binary(), options()) -> binary().
quote_bin_to_bin(Bin, _Options) when is_binary(Bin) ->
    quote_bin_to_bin(Bin, _Options, <<>>).

-spec quote_bin_to_bin(binary(), options(), binary()) -> binary().
quote_bin_to_bin(<<AC, T/binary>>, _Options, Acc) ->
    case is_url_safe(AC) of
        true ->
            quote_bin_to_bin(T, _Options, <<Acc/binary, AC>>);
        false ->
            H = tohex(highbits(AC)),
            L = tohex(lowbits(AC)),
            quote_bin_to_bin(T, _Options, <<Acc/binary, $%, H, L>>)
    end;
quote_bin_to_bin(<<>>, _Options, Acc) ->
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
