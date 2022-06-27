%-----------------------------------------------------------------------------%
%
% File: sample.m
% Main author: Sean Charles
% Date: Sun Jun 26 09:35:24 2022
%
% Various small samples of using the string module.
%
%-----------------------------------------------------------------------------%
:- module sample.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = [ A0 | _ ] then
        Sample = A0
    else
        % The smiley is UTF-8 and will show up in string_lengths.
        Sample = "A quick brown fox jumps over the lazy dog.  😀"
    ),

    string_lengths(Sample, !IO),
    string_conversions(Sample, !IO),
    string_hashes(Sample, !IO),
    string_reading(Sample, !IO),
    string_format_table(Sample, !IO)
    .


%----------------------------------------------------------------------------%

    % String length functions
    %
:- pred string_lengths(string::in, io::di, io::uo) is det.

string_lengths(Sample, !IO) :-
    underline("String lengths", !IO),
    CodeUnitLength1 = string.length(Sample),
    CodeUnitLength2 = string.count_code_units(Sample), % synonym for length()
    CodePointCount  = string.count_codepoints(Sample),

    % In this large format string, not how the compiler allows you to
    % have multiple long lines in a very intuitive manner.

    io.format("\
Try passing an argument string with UTF-8 characters e.g. 😀
to see different values for code 'units' and code 'points'.

string.length:           %i
string.count_code_units: %i
string.count_codepoints: %i
\n",
        [i(CodeUnitLength1), i(CodeUnitLength2), i(CodePointCount)],
        !IO
    ).


%----------------------------------------------------------------------------%

    % Conversions between strings and lists of characters
    %
:- pred string_conversions(string::in, io::di, io::uo) is det.

string_conversions(Sample, !IO) :-
        % Note use of string.format() as the argument
    underline(
        string.format(
            "String to List of Characters: len = %i",
            [i(string.length(Sample))]
         ),
        !IO
    ),
    string.to_char_list(Sample, FwdList),
    string.to_rev_char_list(Sample, RevList),

    % Generate string representations of the lists.
    % Note the use os string.string() to produce a string representation
    % of a term; this is a VERY usefule call to know about.

    StringFwd = string(FwdList),
    StringRev = string(RevList),

    io.format("\n> list of chars:\n", [], !IO),
    io.print_line(StringFwd, !IO),

    io.format("\n> reverse list of chars:\n", [], !IO),
    io.print_line(StringRev, !IO),

    % UTF code point generaetion: string to list(int)!

    string.to_utf8_code_unit_list(Sample, Utf8),
    io.format("\nSample as a list of utf-8 code points:\n%s\n",
        [s(string(Utf8))], !IO).

%----------------------------------------------------------------------------%

    % String hash demonstration.
    %
:- pred string_hashes(string::in, io::di, io::uo) is det.

string_hashes(Sample, !IO) :-
    io.nl(!IO),
    underline("String hashes", !IO),
    io.format("\
hash:  %i
hash2: %i
hash3: %i
hash4: %i
hash5: %i
\n",
        [ i(string.hash(Sample)), i(string.hash2(Sample)),
          i(string.hash3(Sample)), i(string.hash4(Sample)),
          i(string.hash5(Sample))
        ],
        !IO
    ).

%----------------------------------------------------------------------------%

    % Reading characters from a string.
    % The det_XXX() predicates ASSUME you already know that your read will NOT
    % fail because of out-of-range values.
    %0
:- pred string_reading(string::in, io::di, io::uo) is det.

string_reading(Sample, !IO) :-
    underline("Reading data from strings", !IO),

    ( if string.index(Sample, 0, CF) then
            % Show two ways to read the same character
        CharFirst0 = CF,
        CharFirst1 = Sample ^elem(0)
    else
        CharFirst0 = '?',
        CharFirst1 = '?'
    ),

    Len = string.length(Sample),
    ( if string.index(Sample, Len-1, CL)
    then CharLast = CL
    else CharLast = '?'
    ),

    % Read the middle part of the string, i.e. not including
    % the first and last parts: i.e. `between`, never fails.

    MiddlePart = string.between(Sample, 1, Len-1),

    % Note: if the last character take more than one code unit it will
    % probably print as the blacked out question mark!

    io.format("\
First character:   %c %c
Last character:    %c
Bit in the middle: %s
\n", [c(CharFirst0), c(CharFirst1), c(CharLast), s(MiddlePart)], !IO),

    % To access all the code points i.e. complete UTF-8 characters in the
    % string you can either split it or iterate it. Splitting is shown in
    % the string_conversions() predicate.

    CodePointLen = string.count_codepoints(Sample),
    io.format("Code points in sample: %i\n", [i(CodePointLen)], !IO),

    % Pred is a 'closure', then we pass it to the string fold.
    % Despite the same name, the !IO is -different-, change the name if
    % you like to something else to experiment.

    Pred = (pred(CodePoint::in, !.IO::di, !:IO::uo) is det :-
        io.write_char(CodePoint, !IO),
        io.write_char(' ', !IO)
    ),

    % This WILL show multiple code-unit characters correctly i.e. if you use
    % the default argument, you will see the smiley face correctly rendered
    % assuming your terminal also renders UTF-8 correctly of course!

    string.foldl(Pred, Sample, !IO),
    io.nl(!IO),
    io.nl(!IO).

%----------------------------------------------------------------------------%

    % use of the format_table() function for output.
    %
:- pred string_format_table(string::in, io::di, io::uo) is det.

string_format_table(Sample, !IO) :-
    underline("Simple table formatting with columns", !IO),
    % Create a table, all the columns MUST be the same length.
    TableLeft = string.format_table(
        [
            left([ "Column 1", Sample,     "Column 3" ]), % column 1
            left([ "Column1",  "Column 2", Sample     ]), % column 2
            left([ Sample,     "Column 2", "Column 3" ])  % column 3
        ],
        " | "  % Column separation string
    ),
    io.print_line(TableLeft, !IO),
    io.nl(!IO),

    underline(" > right justified", !IO),
    TableRight = string.format_table(
        [
            right([ "Column 1", Sample,     "Column 3" ]),
            right([ "Column1",  "Column 2", Sample     ]),
            right([ Sample,     "Column 2", "Column 3" ])
        ],
        " | "  % Column separation string
    ),
    io.print_line(TableRight, !IO),
    io.nl(!IO).

%----------------------------------------------------------------------------%

    % Given a string, underline it with matching '=' characters.
    %
:- pred underline(string::in, io::di, io::uo) is det.

underline(S, !IO) :-
    io.print_line(S, !IO),
    io.print_line(
        % :string tells the compiler that we mean to use the function version
        % of dupplicate_char, not the pred() version, which would cause the
        % compiler to get confused... do we mean the func() version or are we
        % trying print a partial application(). If you want to see for yourself
        % then remove the `:string` and try to compile the code again.
        string.duplicate_char('=', string.length(S)) :string,
        !IO
    ).

%----------------------------------------------------------------------------%
:- end_module sample.
%----------------------------------------------------------------------------%

