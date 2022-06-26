# What's here ?

I've been learning to use Mercury for about two years now, using it pretty
much every day for an hour or three, towards building a language transpiler,
but that's another story! Along the way, I soon found out that not many people
have heard of Mercury, it started life in 1995, three years before Haskell.
It's time is coming, especially if I can help the cause.

While parts of it are still almost a complete mystery to me (inst, modes,
higher-order things) I have however built up a usable working knowledge of it
to the point where I think I might at least be able to help others learn it.

The one thing that I found hard to find was good clear concise examples of
some of the features of the language, the official documentation is the
definitive reference and as such, not particularly friendly towards people
learning the language who may not always understand the terminology. Some of
it is very high-level, at least for me, althought learning it has really
improved my skills and knowledge, and continues to do so.


## Mercury Modules - Sample Code

To that end, and to satisfy my own learning, I have decided to formally start
working my way through the libraries that I use a lot, and some I don't, and
produce some simple working examples using them. This will serve as both a
source of learning materials for some, and reference and revision for me!


## Building

Where possible, I have kept to the same format in all folders, a single file
called `sample.m` which can be built with:

    $ mmc sample

and then run as:

    $ ./sample


## Running

Before running I encourage you to read the source code as some sample programs
will have command line options and/or values that you can pass to affect the
output of the sample.


## Refactoring Exercise

The implementation of greater_than, less_than and equal_to is deliberately
boiler plated, if you feel brave, refactor it to have common ancestry! It's
really not that hard, I almost did it myself but then the code would not have
been so obvious as it is to read.

Thanks.
