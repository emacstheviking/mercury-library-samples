# Directed Clause Grammar Notation

Mercury implements the expected set of DCG rewrite rules should you be
experienced with such things from Prolog world. It also has a few extras as
well. According to the manual, DCG notation should be used for parsers and
generators, anything else should really be using the new 'state variable'
syntax.

From the manual:

https://mercurylang.org/information/doc-latest/mercury_ref/DCG_002drules.html#DCG_002drules

>Definite Clause Grammar notation is intended for writing parsers and sequence
>generators in a particular style; in the past it has also been used to thread
>an implicit state variable, typically the I/O state, through code. As a
>matter of style, we recommend that in future DCG notation be reserved for
>writing parsers and sequence generators, and that state variable syntax be
>used for passing state threads.


So there you go, a matter of style!

I am not going to even try to start explaining what DCG notation is or how it
works; I am presuming you already have a reasonable idea and just want to see
how what you already know translates into Mercury, for the most part it's
identical.


## Building

It couldn't be simpler:

    $ mmc sample.m

## Running it

    $ ./sample "string to parse"

The input string can either be the word "help" or one of the following three
questions where N1, N2 are positive integers i.e the digits 0-9 only, just to
keep it simple.

Here's some sample input and output sessions:

    ➜  DCG git:(main) ✗ ./sample "is 42 less than 40 ?"
    No, 42 isn't less than 40

    ➜  DCG git:(main) ✗ ./sample "is 42 greater than 32?"
    Yes, 42 is greater than 32

    ➜  DCG git:(main) ✗ ./sample "is 42 equal to 18 ?"
    No, 42 isn't the same as 18

    ➜  DCG git:(main) ✗ ./sample "is 42 equal to 42 ?"
    Yes, 42 is equal to 42


The parsing is whitespace tolerant, I've commented a lot of things here and
there and I hope you find this as a clear and simple use of DCG notation with
Mercury.

## Refactoring Exercise

The implementation of greater_than, less_than and equal_to is deliberately
boiler plated, if you feel brave, refactor it to have common ancestry! It's
really not that hard, I almost did it myself but then the code would not have
been so obvious as it is to read.
