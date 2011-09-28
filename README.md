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

    quoted:make([opt()]) -> options().
    -type opt()
       :: {charcase, upper|lower}
        | {strict, true|false}
        | {unsafe, keep|crash}
        | {plus, true|false}.


#### charcase

    {charcase, upper|lower}

    default for decoding: not used
    default for encoding: 'upper'

The `charcase` option specifies the character case to use when encoding
unsafe characters as hexadecimal numbers. If the option value is `upper`
characters in the range __A-F__ are used. If the option value is `lower`
characters in the range __a-f__ are used.


#### strict

    {strict, true|false}

    default for decoding: 'true'
    default for encoding: not used

The `strict` option speficies the desired behaviour when encountering a percent
character not followed by a hexadecimal number. If the option value is `true`
a badarg error is thrown. If the option value is `false` the percent character
is included in the output.


#### unsafe

    {unsafe, keep|crash}

    default for decoding: 'false'
    default for encoding: not used

The `unsafe` option specifies the desired behaviour when encountering an unsafe
character in the input while decoding a quoted string. If the option value is
`keep` the unsafe character is included in the output and if the option value
is `crash` a badarg error is thrown.


#### plus

    {plus, true|false}

    default for decoding: 'true'
    default for encoding: 'false'

The `plus` option specifies the desired behaviour when encountering a `'+'` or
`' '` character. If the option value is `true`, `'+'` is used as an alias for the
`' '` character, `'+'` is decoded as `' '` and `' '` is encoded as `'+'`.
If the option value is `false`, `'+'` is decoded as `'+'` and `' '` is encoded as `'%20'`.


[![Build Status](http://travis-ci.org/klaar/quoted.erl.png?branch=master)](http://travis-ci.org/klaar/quoted.erl)
