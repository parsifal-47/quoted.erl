# API

## Decoding

    quoted:from_url(binary()) -> binary().
    quoted:from_url([byte()]) -> [byte()].

    quoted:from_url(binary(), options()) -> binary().
    quoted:from_url([byte()], options()) -> [byte()].

## Encoding

    quoted:to_url(binary()) -> binary().
    quoted:to_url([byte()]) -> [byte()].

    quoted:to_url(binary(), options()) -> binary().
    quoted:to_url([byte()], options()) -> [byte()].

## Options

    -type opt()
       :: {charcase, upper|lower}
        | {strict, true|false}
        | {plus, true|false}.

    quoted:make([opt()]) -> options().

#### charcase

The 'charcase' option specifies the character case to use when encoding
unsafe characters as hexadecimal numbers. If the option value is 'upper'
characters in the range "A-F" are used. If the option value is 'lower'
characters in the range "a-f" are used.

    default for decoding: not used
    default for encoding: 'upper'

#### strict

The 'strict' option speficies the desired behaviour when encountering a percent
character not followed by a hexadecimal number. If the option value is 'true'
a badarg error is thrown. If the option value is 'false' the percent character
is included in the output.

    default for decoding: 'true'
    default for encoding: not used

#### plus


The 'plus' option specifies the desired behaviour when encountering a '+' or
' ' character. If the option value is 'true', '+' is used as an alias for the
' ' character, '+' is decoded as ' ' and ' ' is encoded as '+'.
If the option value is 'false, '+' is decoded as '+' and ' ' is encoded as '%20'.

    default for decoding: 'true'
    default for encoding: 'false'

[![Build Status](http://travis-ci.org/klaar/quoted.erl.png?branch=master)](http://travis-ci.org/klaar/quoted.erl)
