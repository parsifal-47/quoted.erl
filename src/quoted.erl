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

%% macros
-define(print(Arg), (begin io:format("~p~n", [Arg]), Arg end)).


-type data() :: [byte()] | binary().
-type option()
   :: {charcase, lower | upper}
    | {unsafe, keep | crash}
    | {strict, boolean()}
    | {plus, boolean()}.

-record(options, {
    lower  = default :: boolean(),
    strict = default :: boolean(),
    unsafe = default :: boolean(),
    plus   = default :: boolean()}).

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
    Lower = case lists:keyfind(charcase, 1, OptionsList) of
        {charcase, lower} -> true;
        {charcase, upper} -> false;
        false -> default;
        _ -> erlang:error(badarg)
    end,
    Strict = case lists:keyfind(strict, 1, OptionsList) of
        {strict, true} -> true;
        {strict, false} -> false;
        false -> default;
        _ -> erlang:error(badarg)
    end,
    Plus = case lists:keyfind(plus, 1, OptionsList) of
        {plus, true} -> true;
        {plus, false} -> false;
        false -> default;
        _ -> erlang:error(badarg)
    end,
    Unsafe = case lists:keyfind(unsafe, 1, OptionsList) of
        {unsafe, keep} -> true;
        {unsafe, crash} -> false;
        false -> default;
        _ -> erlang:error(badarg)
    end,
    #options{lower=Lower, strict=Strict, unsafe=Unsafe, plus=Plus}.


%% @private Return the default decoding options.
decode_defaults() ->
    #options{lower=true, strict=true, unsafe=true, plus=true}.


%% @private Return the default encoding options.
encode_defaults() ->
    #options{lower=true, strict=true, unsafe=true, plus=false}.

%% @private Replace all occurances of 'default' with the default value.
merge_options(Opts, _Def) when
Opts#options.lower  =/= default, Opts#options.strict =/= default,
Opts#options.unsafe =/= default, Opts#options.plus   =/= default ->
    Opts;
merge_options(Opts, Def) when
Opts#options.lower  =:= default, Opts#options.strict =:= default,
Opts#options.unsafe =:= default, Opts#options.plus   =:= default ->
    Def;
merge_options(Opts, Def) ->
    #options{lower=Lower, strict=Strict, unsafe=Unsafe, plus=Plus} = Opts,
    UseLower  = case Lower  of default -> Def#options.lower;  _ -> Lower end,
    UseStrict = case Strict of default -> Def#options.strict; _ -> Strict end,
    UseUnsafe = case Unsafe of default -> Def#options.unsafe; _ -> Unsafe end,
    UsePlus   = case Plus   of default -> Def#options.plus;   _ -> Plus end,
    #options{lower=UseLower, strict=UseStrict, unsafe=UseUnsafe, plus=UsePlus}.


%% @doc
%% @end
-spec to_url(data()) -> data().
to_url(Data) ->
    to_url_(Data).


to_url_(Data) ->
    to_url_(Data, encode_defaults()).


%% @doc
%% @end
-spec to_url(data(), options()) -> data().
to_url(Data, Options) ->
    to_url_(Data, Options).


to_url_(Str, Options) when is_list(Str) ->
    quote_list_to_list(Str, merge_options(Options, encode_defaults()));
to_url_(Bin, Options) when is_binary(Bin) ->
    quote_bin_to_bin(Bin, merge_options(Options, encode_defaults())).


%% @doc
%% @end
-spec from_url(data()) -> data().
from_url(Data) ->
    from_url_(Data).


from_url_(Data) ->
    from_url_(Data, decode_defaults()).


%% @doc
%% @end
-spec from_url(data(), options()) -> data().
from_url(Data, Options) ->
    from_url_(Data, Options).


from_url_(Str, Options) when is_list(Str) ->
    unquote_list_to_list(Str, merge_options(Options, decode_defaults()));
from_url_(Bin, Options) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin, merge_options(Options, decode_defaults())).


-spec unquote_list_to_list([byte()], options()) -> [byte()].
unquote_list_to_list([$%|HT=[HH,HL|T]], Options) ->
    H = unhex(HH),
    L = unhex(HL),
    case H =:= error orelse L =:= error of
        true when Options#options.strict ->
            erlang:error(badarg);
        true ->
            [$%|unquote_list_to_list(HT, Options)];
        false ->
            C = tobyte(H, L),
            [C|unquote_list_to_list(T, Options)]
    end;
unquote_list_to_list([$%|_], Options) when Options#options.strict ->
    erlang:error(badarg);
unquote_list_to_list([$+|T], Options) when Options#options.plus ->
    [$ |unquote_list_to_list(T, Options)];
unquote_list_to_list([C|T], Options) when Options#options.unsafe ->
    [C|unquote_list_to_list(T, Options)];
unquote_list_to_list([C|T], Options) ->
    case is_url_safe(C) of
        false -> erlang:error(badarg);
        true -> [C|unquote_list_to_list(T, Options)]
    end;
unquote_list_to_list([], _Options) ->
    [].


-spec unquote_bin_to_bin(binary(), options()) -> binary().
unquote_bin_to_bin(Bin, Options) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin, Options, <<>>).

-spec unquote_bin_to_bin(binary(), options(), binary()) -> binary().
unquote_bin_to_bin(<<$%,HH,HL,T/binary>>, Options, Acc) ->
    H = unhex(HH),
    L = unhex(HL),
    case H =:= error orelse L =:= error of
        true when Options#options.strict ->
            erlang:error(badarg);
        true ->
            unquote_bin_to_bin(<<HH,HL,T/binary>>, Options, <<Acc/binary, $%>>);
        false ->
            C = tobyte(H, L),
            unquote_bin_to_bin(T, Options, <<Acc/binary, C>>)
    end;
unquote_bin_to_bin(<<$%, _/binary>>, Options, _Acc) when Options#options.strict ->
    erlang:error(badarg);
unquote_bin_to_bin(<<$+, T/binary>>, Options, Acc) when Options#options.plus ->
    unquote_bin_to_bin(T, Options, <<Acc/binary, $ >>);
unquote_bin_to_bin(<<C, T/binary>>, Options, Acc) when Options#options.unsafe ->
    unquote_bin_to_bin(T, Options, <<Acc/binary, C>>);
unquote_bin_to_bin(<<C, T/binary>>, Options, Acc) ->
    case is_url_safe(C) of
        false -> erlang:error(badarg);
        true -> unquote_bin_to_bin(T, Options, <<Acc/binary, C>>)
    end;
unquote_bin_to_bin(<<>>, _Options, Acc) ->
    Acc.


-spec quote_list_to_list([byte()], options()) -> [byte()].
quote_list_to_list([$ |T], Options) when Options#options.plus ->
    [$+|quote_list_to_list(T, Options)];
quote_list_to_list([AC|T], Options) ->
    case is_url_safe(AC) of
        true ->
            [AC|quote_list_to_list(T, Options)];
        false ->
            H = tohex(highbits(AC), Options),
            L = tohex(lowbits(AC), Options),
            [$%,H,L|quote_list_to_list(T, Options)]
    end;
quote_list_to_list([], _Options) ->
    [].


-spec quote_bin_to_bin(binary(), options()) -> binary().
quote_bin_to_bin(Bin, _Options) when is_binary(Bin) ->
    quote_bin_to_bin(Bin, _Options, <<>>).

-spec quote_bin_to_bin(binary(), options(), binary()) -> binary().
quote_bin_to_bin(<<$ , T/binary>>, Options, Acc) when Options#options.plus ->
    quote_bin_to_bin(T, Options, <<Acc/binary, $+>>);
quote_bin_to_bin(<<AC, T/binary>>, Options, Acc) ->
    case is_url_safe(AC) of
        true ->
            quote_bin_to_bin(T, Options, <<Acc/binary, AC>>);
        false ->
            H = tohex(highbits(AC), Options),
            L = tohex(lowbits(AC), Options),
            quote_bin_to_bin(T, Options, <<Acc/binary, $%, H, L>>)
    end;
quote_bin_to_bin(<<>>, _Options, Acc) ->
    Acc.


-spec highbits(byte()) -> byte().
highbits(I) -> I bsr 4.

-spec lowbits(byte()) -> byte().
lowbits(I)  -> I band 16#0F.

-spec tobyte(byte(), byte()) -> byte().
tobyte(H, L) -> L bor (H bsl 4).

-spec tohex(byte(), options()) -> byte().
tohex(C, Options) ->
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
        _ when Options#options.lower ->
            case C of
                10 -> $a;
                11 -> $b;
                12 -> $c;
                13 -> $d;
                14 -> $e;
                15 -> $f
            end;
        _ when not Options#options.lower ->
            case C of
                10 -> $A;
                11 -> $B;
                12 -> $C;
                13 -> $D;
                14 -> $E;
                15 -> $F
            end
    end.

-spec unhex(byte()) -> byte() | error.
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
        $F -> 15; $f -> 15;
        _ -> error
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
