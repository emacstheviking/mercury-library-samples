# Foreign Function Interface

Having already done a fair bit of "C" interfacing to the library 'SDL2' I
wanted to explore the nooks and crannies and get really comfortable with how
one creates code in "C" that can call back into Mercury and also how to call
"C" code from Mercury. Also this is an exploration into passing datatypes as
well, for my needs I wanted to explore simple "C" structure passing and value
passing back and forth just to explore the options open to me.

This particular example shows how you can call the "C" function `stat()` to
obtain some information about a file. I needed this as I wanted to implement a
very naive "file watcher" whereby I will monitor the files being watched about
once a second and if the modification time has changed then I know that I need
to re-process the file.


## Building

It couldn't be simpler:

    $ mmc sample.m

## Running it

    $ ./sample FILENAME

The input is the source file you wan to use as the target to the "C" function
`stat()`. We use it to return the filesize and the three available times from
the underlying operating system. The final version returns a fully populated
Mercury type we defined called `statinfo.
