%-----------------------------------------------------------------------------%
%
% File: sample.m
% Main author: Sean Charles
% Date: Sun Jun 26 13:41:41 2022
%
% DCG notation with Mercury.
%
% I have also thrown in some uses of the trace[] statement to show how you
% can output information to the output stream even in a predicate that does
% not have it in scope.
%
%-----------------------------------------------------------------------------%
:- module sample.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.


main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = [ A0 ] then
        parse_input(A0, Command),
        eval_command(Command, !IO)
    else
        show_options(!IO)
    ).

%-----------------------------------------------------------------------------%

    % Execute a parsed command.
    %
:- pred eval_command(command::in, io::di, io::uo) is det.

eval_command(help, !IO) :-
    show_options(!IO).

eval_command(greater_than(N1, N2), !IO) :-
    ( if N1 > N2 then
        io.format("Yes, %i is greater than %i\n",
            [i(N1), i(N2)], !IO)
    else
        io.format("No, %i isn't greater than %i\n",
            [i(N1), i(N2)], !IO)
    ).

eval_command(less_than(N1, N2), !IO) :-
    ( if N1 < N2 then
        io.format("Yes, %i is less than %i\n",
            [i(N1), i(N2)], !IO)
    else
        io.format("No, %i isn't less than %i\n",
            [i(N1), i(N2)], !IO)
    ).

eval_command(equal_to(N1, N2), !IO) :-
    ( if N1 = N2 then
        io.format("Yes, %i is equal to %i\n",
            [i(N1), i(N2)], !IO)
    else
        io.format("No, %i isn't the same as %i\n",
            [i(N1), i(N2)], !IO)
    ).

%-----------------------------------------------------------------------------%

    % Parse the command line input.
    % To be effective, make sure you use "..." on the command line as only
    % the first string will be used for the parsing process.
    %
:- pred parse_input(string::in, command::out) is det.

parse_input(Input, Command) :-
    % To keep it simple, we will first convert the input string to lower
    % case as this will not affect the numbers in the input.

    Text = string.to_lower(Input):string,

    % A DCG traditionally (as in this case) expects a list(T) in and will
    % return anything un-consumed as a list(T) in turn. For this parser we
    % will convert our string into a list of UTF-8 code points.
    %
    % The output here is '_' because we are only looking for a match on
    % the input; to be really pedantic you could put '[]' instead which
    % means that the remaining input MUST be empty i.e. the source was
    % FULLY PARSED with nothing left over.

    parse_text(Command, string.to_char_list(Text), _).


    % This is the returned command from the parser; it will either be the
    % help command on failure to parse (or you typed help!) or else it will
    % be a command representative of your input.
    %
:- type command
    --->    help
    ;       greater_than(int, int)
    ;       less_than(int, int)
    ;       equal_to(int, int).


    % Here we alias the type 'list(char)' to be 'lc' as a short-cut to
    % having to type it all the time. This can also be used as a level of
    % indirection against change. That is, if you decided you had to change
    % ALL your parsing code to take list(X) instead of list(Y) then you
    % only have to change the definition of 'lc' to help minimise effort.
    %
:- type lc == list(char).

    % Parse the input character data.
    % Det means we won't fail; we will always return a result.
    %
:- pred parse_text(command::out, lc::in, lc::out) is det.

parse_text(Command) -->
    ( if help_command then
        { Command = help }

    else if greater_than(N1, N2) then
        { Command = greater_than(N1, N2) }

    else if less_than(N1, N2) then
        { Command = less_than(N1, N2) }

    else if equal_to(N1, N2) then
        { Command = equal_to(N1, N2) }

    else
        { Command = help}
    ).

    % Is this the HELP command ?
    % semidet means it MIGHT fail if we don't have a match.
    %
:- pred help_command(lc::in, lc::out) is semidet.

help_command --> ['h', 'e', 'l', 'p'].


    % Find a sentence matching 'is X greater than Y?'.
    % The terminating ? is required. Note we use a common parser called
    % a_word to actually make it easier because don't forget that the
    % input will contain whitespace in between the words as well!
    %
:- pred greater_than(int::out, int::out, lc::in, lc::out) is semidet.

greater_than(N1, N2) -->
    a_word("is"),
    a_number(N1),
    {
        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("got N1: %i\n", [i(N1)], !Dbg)
        )
    },
    a_word("greater"),
    a_word("than"),
    a_number(N2),
    {
        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("got N2: %i\n", [i(N2)], !Dbg)
        )
    },
    a_word("?").

:- pred less_than(int::out, int::out, lc::in, lc::out) is semidet.

less_than(N1, N2) -->
    a_word("is"),
    a_number(N1),
    {
        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("got N1: %i\n", [i(N1)], !Dbg)
        )
    },
    a_word("less"),
    a_word("than"),
    a_number(N2),
    {
        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("got N2: %i\n", [i(N2)], !Dbg)
        )
    },
    a_word("?").


:- pred equal_to(int::out, int::out, lc::in, lc::out) is semidet.

equal_to(N1, N2) -->
    a_word("is"),
    a_number(N1),
    {
        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("got N1: %i\n", [i(N1)], !Dbg)
        )
    },
    a_word("equal"),
    a_word("to"),
    a_number(N2),
    {
        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("got N2: %i\n", [i(N2)], !Dbg)
        )
    },
    a_word("?").


:- pred a_word(string::in, lc::in, lc::out) is semidet.

a_word(Word) -->
    % get current input list, see if it starts with Word,
    % if it does... we are good to slice and return
    % The =() captures the input argument to the DCG rule into
    % the named variable, in this case InputChars.

    skip_ws,
    =(InputChars),

    {
        InputString = string.from_char_list(InputChars),

        % This is how to put debug print statements anywhere you need
        % through using trace[]. It brings into scope an io.state and
        % for consistency we call it !IO but it can be anything you'd
        % like to call it. The code inside the parens can then use it
        % to write something to the output.
        %
        % To see this output, you must set the environment variable
        % to exist, e.g. on Linux or Mac you can do this:
        %
        %     OPEN_SESAME= ./sample "is 10 greater than 5 ?"

        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("a_word: WORD:  ""%s"", INPUT: ""%s""\n",
                [s(Word), s(string(InputChars))], !Dbg)
        )
    },

    % remove_prefix is 'semidet', so if it fails, the Word is not
    % at the start of the string after all.
    { string.remove_prefix(Word, InputString, Remainder) },

    % If we get this far then Word was the prefix, and Remainder
    % is the new 'rest of the input buffer' so we can use the DCG
    % function '' to make it the output from this DCG rule.

    { OutputChars = string.to_char_list(Remainder) },
    :=(OutputChars),

    { trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("a_word: OUTPUT: ""%s""\n",
                [s(string(Remainder))], !Dbg)
        )
    }.

%-----------------------------------------------------------------------------%

    % Extract an integer number from the source.
    % It can be any unbroken sequence of digits 0-9.
    %
:- pred a_number(int::out, lc::in, lc::out) is semidet.

a_number(N) -->
    skip_ws,
    read_number(Chars),
    {
        String = string.from_char_list(Chars),
        string.to_int(String, N),

        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("a_number: ""%s"", value: %i\n",
                [s(String), i(N)], !Dbg)
        )
    }.

%-----------------------------------------------------------------------------%

    % Read a sequence of digits.
    % In this example, the final number is built up by first extracting the
    % next digit, then we call ourselves again. This feels odd at first, but
    % once read_number terminates, as the stack frames are unwound back to
    % the top, the list is composed from the individual digits obtained on
    % each pass through where digit//1 succeeded.
    %
    % Run with OPEN_SESAME to see it work the magic!
    %
:- pred read_number(list(char)::out, list(char)::in,
    list(char)::out) is semidet.

read_number([ Digit | Rest ]) -->
    % Scan for a digit, this CAN fail. so if this is the FIRST time
    % this was called, we aren't 'on' a number. If this is not the
    % first time then we have been accumulating at least one digit
    % and now we have just hit the first non-digit character.

    digit(Digit),

    {
        trace[io(!Dbg), runtime(env("OPEN_SESAME"))]
        (
            io.format("read_number: Digit: %c, Rest: %s\n",
                [c(Digit), s(string(Rest))], !Dbg)
        )
    },

    ( if read_number(Digits) then
        { Rest = Digits }
    else
        { Rest = [] }
    ).


    % Is the next character a digit ?
    %
:- pred digit(char::out, lc::in, lc::out) is semidet.

digit(C) -->
    [ C ],
    { char.is_digit(C) }.

%-----------------------------------------------------------------------------%

    % Skip whitespace in the input text.
    % We rely on the char.is_whitespace implementation for this.
    %
:- pred skip_ws(lc::in, lc::out) is det.

skip_ws -->
    (   ws -> skip_ws
            % recursively call ourselves to keep skipping
    ;
        []  % end of whitespace, succeed. Using [] in a DCG is
            % the same as using { true } here.
    ).


    % Succeeds if next character is whitespace.
    %
:- pred ws(lc::in, lc::out) is semidet.

ws -->
    [ C ],
    { char.is_whitespace(C) }.


%-----------------------------------------------------------------------------%

    % Show the usage instructions.
    % Note: See how "" resolves to a single " in a Mercury string.
    %
:- pred show_options(io::di, io::uo) is det.

show_options(!IO) :-
    io.format("\
The SINGLE string argument to this program can be one of:

    help - shows this output.
    is <NUMBER1> [less than/greater than/equal to] <NUMBER2> ?

Hint: Use """""""" to ensure spaces are passed through from the command line.

", [], !IO).



%----------------------------------------------------------------------------%
:- end_module sample.
%----------------------------------------------------------------------------%

