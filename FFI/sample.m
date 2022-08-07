%-----------------------------------------------------------------------------%
%
% File: sample.m
% Main author: Sean Charles
% Date: Sat Aug  6 09:14:32 2022
%
% Demonstrating the interplay between Mercury and "C" when using the
% High-Level C grade. Other grades may or may not play ball, you'd have to
% read the manual for exact enlightenment.
%
% This file starts of with filestat1(), a very simple example of how to call
% into "C" world from Mercury, then as we progress through to filestat4() we
% get progressively more daring by creating a more realistic return structure,
% finally we end up with an io.res(T) result that contains a `statinfo` type
% we constructed earlier.
%
%-----------------------------------------------------------------------------%
:- module sample.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module calendar.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    Arg = string.append_list(Args),
    io.format("stat on: %s\n", [s(Arg)], !IO),

        % version 1: just see if the file is good
    filestat1(Arg, !IO),

        % version 2: simplistic return of error
    filestat2(Arg, Error, !IO),
    io.format("filestat2: error: %i\n", [i(Error)], !IO),

        % version 3: a more Mercury like response
    filestat3(Arg, F3res, !IO),
    io.format("filestat3: response: %s\n", [s(string(F3res))], !IO),

        % version 4: a more Mercury like response, but slicker such that
        % we can do the usual disjunctive switch on the result.
    filestat4(Arg, F4res, !IO),
    (
        F4res = error(E),
        io.format("filestat4: error: %s\n", [s(io.error_message(E))], !IO)
    ;
        F4res = ok(Info),
        io.print_line(Info, !IO)
    ).
%    io.format("filestat4: response: %s\n", [s(string(F4res))], !IO).

%----------------------------------------------------------------------------%
%   FFI
%----------------------------------------------------------------------------%
:- pragma foreign_decl("C", "#include <sys/stat.h>").
:- pragma foreign_decl("C", "#include <stdio.h>").
:- pragma foreign_decl("C", "#include <time.h>").

    % This is where we define the Mercury predicate 'filestat' which is
    % what our client code would call to interrogate the files status.
    %
    % filestat1: Just establishes the pattern, prints out the results
    % to the stdout stream from inside the "C" code to show what is
    % going on.
    %
:- pred filestat1(string::in, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", filestat1(File::in, _IO0::di, _IO::uo),
    [   promise_pure
    ,   will_not_call_mercury
    ,   thread_safe
    ,   will_not_modify_trail
    ,   does_not_affect_liveness
    ,   tabled_for_io
    ],
    "
    struct stat buffer;
    int         status;

    status = stat(File, &buffer);
    if (!status) {
        printf(""ffi:stat(%s) ok\\n"", File);
    }
    else {
        printf(""ffi:stat(%s) failed: %s\\n"", File, strerror(errno));
    }
    "
).
    % OK, the --next-- stage of development might be to be able to return a
    % simple value to indicate if the called succeeded; 0 meaning OK otherwise
    % we might choose to return the "C" errno variable.
    %
:- pred filestat2(string::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", filestat2(File::in, Error::out, _IO0::di, _IO::uo),
    [   promise_pure
    ,   will_not_call_mercury
    ,   thread_safe
    ,   will_not_modify_trail
    ,   does_not_affect_liveness
    ,   tabled_for_io
    ],
    "
    struct stat buffer;
    int         status;
    status = stat(File, &buffer);
    if (status) Error = errno; else Error = 0;
    "
).
    % More adventurous...can we return io.error on fail?
    % First we define a Mercury function that takes an error code the
    % error string (valid only if error code not zero), and then it
    % can return the required type.
    %
:- func make_error(int::in, string::in) = (io.res::out) is det.

make_error(E, Why) = Out :-
    ( if E = 0 then
        Out = ok
    else
        Out = io.error(io.make_io_error(Why))
    ).

:- pragma foreign_export("C", make_error(in, in) = (out), "makeError").


:- pred filestat3(string::in, io.res::out, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", filestat3(File::in, Error::out, _IO0::di, _IO::uo),
    [   promise_pure
    ,   thread_safe
    ,   will_not_throw_exception
    ,   will_not_modify_trail
    ,   does_not_affect_liveness
    ,   tabled_for_io
    ],
    "
    struct stat buffer;
    int         status;

    status = stat(File, &buffer);
    if (status) {
        Error = makeError(errno, strerror(errno));
    }
    else {
        Error = makeError(0,"""");
    }
    "
).
    % Getting smarter with return type; this time we'll upgrade to a more
    % informative io.res(T), and this time the 'T' will contain some of the
    % information we need from the actual stat() call, at last!
    % So to summarise, if the call fails we expect to get back an error()
    % response, else we will get out stat() information, for that we will
    % need to define a type:
    %
:- type statinfo
    --->    statinfo(
                si_size  ::int,
                si_atime ::date,
                si_mtime ::date,
                si_ctime ::date
            ).

    % Given the parts of a 'tm' structure we want to return a date
    % calendar for a more Mercury friendly final representation.
    % We are using the 'det_' versions of the calendar functions as
    % we know the data is coming from a system call and therefore it
    % jolly well ought to be trustworthy in terms of values.
    %
:- func to_date(int::in, int::in, int::in, int::in, int::in, int::in)
    = (date::out) is det.

to_date(Yr, Mo, Dy, Hr, Mi, Se) = Date :-
    Month = calendar.det_int_to_month(Mo),
    Date = calendar.det_init_date(Yr, Month, Dy, Hr, Mi, Se, 0).

:- pragma foreign_export("C", to_date(in, in, in, in, in, in)
    = (out), "toDate").


    % Make the io.res(statinfo) response given the values from the
    % stat structure for the file.
    %
:- func make_response(int::in, date::in, date::in, date::in)
    = (io.res(statinfo)::out) is det.

make_response(Size, ATime, MTime, CTime)
    = ok(statinfo(Size, ATime, MTime, CTime)).

:- pragma foreign_export("C", make_response(in, in, in, in)
    = (out), "statResponse").

    % filestat4: Obtain stat() info for the file or return an error.
    % The returned information contains proper calendar.date objects instead
    % of just the Epoch time, this makes it more Mercury friendly!
    %
:- pred filestat4(string::in, io.res(statinfo)::out, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", filestat4(File::in, Error::out, _IO0::di, _IO::uo),
    [   promise_pure
    ,   thread_safe
    ,   will_not_throw_exception
    ,   will_not_modify_trail
    ,   does_not_affect_liveness
    ,   tabled_for_io
    ],
    "
    struct stat buffer;
    int         status;

    status = stat(File, &buffer);
    if (status) {
        Error = makeError(errno, strerror(errno));
    }
    else {
        struct tm atime, mtime, ctime;
        localtime_r(&buffer.st_atime, &atime);
        localtime_r(&buffer.st_mtime, &mtime);
        localtime_r(&buffer.st_ctime, &ctime);

        Error = statResponse(
            buffer.st_size,
            toDate(
                atime.tm_year, atime.tm_mon, atime.tm_mday,
                atime.tm_hour, atime.tm_min, atime.tm_sec
            ),
            toDate(
                mtime.tm_year, mtime.tm_mon, mtime.tm_mday,
                mtime.tm_hour, mtime.tm_min, mtime.tm_sec
            ),
            toDate(
                ctime.tm_year, ctime.tm_mon, ctime.tm_mday,
                ctime.tm_hour, ctime.tm_min, ctime.tm_sec
            )
        );
    }
    "
).

%----------------------------------------------------------------------------%
:- end_module sample.
%----------------------------------------------------------------------------%

