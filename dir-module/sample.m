%-----------------------------------------------------------------------------%
%
% File: sample.m
% Main author: Sean Charles
% Date: Fri Jun 24 19:15:56 2022
%
% Show off the various predicates and functions in the `dir` module.
%
% NOTE: Non-exhaustive coverage, it's the ones I needed to learn.
%
%-----------------------------------------------------------------------------%
:- module sample.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module dir.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.


    % Main entry point.
    %
    % You MAY pass a single string which will be used as the sample
    % pathname to pass through certain of the sample calls.
    %
main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if [A0 | _ ] = Args
    then Source = A0
    else Source = "/path/to-the/file/hello.txt"
    ),

    % Get the current folder, of fallback to "."

    dir.current_directory(CwdRes, !IO),
    (
        CwdRes = ok(Dir),
        Cwd = Dir
    ;
        CwdRes = error(E),
        io.format("setting Cwd to '.', because: %s",
            [s(io.error_message(E))], !IO),
        Cwd = "."
    ),
    io.format("current working directory: %s\n\n", [s(Cwd)], !IO),


    % Basic directory information

    Sep = dir.directory_separator,
    ThisDir = dir.this_directory,
    ParentDir = dir.parent_directory,

    io.format("directory separator: %c\n", [c(Sep)], !IO),
    io.format("     this directory: %s\n", [s(ThisDir)], !IO),
    io.format("   parent directory: %s\n", [s(ParentDir)], !IO),


    % path_name_is_root_directory

    ( if dir.path_name_is_root_directory(Source) then
        io.format("path_name_is_root_directory: %s Yes!\n\n",
            [s(Source)], !IO)
    else
        io.format("path_name_is_root_directory: %s No!\n\n",
            [s(Source)], !IO)
    ),


    % path_name_is_absolute

    ( if dir.path_name_is_absolute(Source) then
        io.format("path_name_is_absolute:       %s Yes!\n\n",
            [s(Source)], !IO)
    else
        io.format("path_name_is_absolute:       %s No!\n\n",
            [s(Source)], !IO)
    ),


    % split_name

    ( if dir.split_name(Source, A, B) then
        io.format("dirname: %s, basename: %s\n\n", [s(A), s(B)], !IO)
    else
        io.format("dirname: %s: a root path was given, can't do that!\n\n",
            [s(Source)], !IO)
    ),


    % basename

    ( if dir.basename(Source, PathName) then
        io.format("basename: %s\n\n", [s(PathName)], !IO)
    else
        io.format("basename failed for: %s\n\n", [s(Source)], !IO)
    ),


    % brace expansion
    % read: https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_04.html

    io.format("Brace expansion:\n", [], !IO),
    Strings = dir.expand_braces("the {goose,CPU,planet} is cooked"),
    list.foldl(io.print_line, Strings, !IO),
    io.nl(!IO),


    % SIMPLE directory walking: dir.recursive_fold2
    %
    % Print out the contents of the -parent- directory which should
    % list all the things within the parent GitHub project!
    % Along the way we can collect something useful(?).

    Visitor = (pred(Dir::in, Basename::in, Type::in, Cont::out,
        !.Info::in, !:Info::out, !.IO::di, !:IO::uo) is det :-

        % signal to continue walking
        Cont = yes,

        ( if display_file(!.Info) = yes then
            io.format(" > %s: %s%c%s\n",
                [s(string(Type)), s(Dir), c(dir.directory_separator),
                s(Basename)], !IO)
        else
            true
        ),

        % count readme files, Mercury source files, and a total files
        % count using the field access functions.

        ( if Basename = "README.md" then
            !:Info = !.Info ^readme_count := !.Info ^readme_count+1
        else
            true
        ),
        ( if string.suffix(Basename, ".m") then
            !:Info = !.Info ^m_count := !.Info ^m_count+1
        else
            true
        ),
        !:Info = !.Info ^file_count := !.Info ^file_count+1
    ),

    io.format("Walking parent folder with dir.recursive_fold2:\n", [], !IO),
    Info0 = info( no, 0, 0, 0 ),

    dir.recursive_foldl2(Visitor, ParentDir, no, Info0, Res, !IO),
    (
        Res = io.ok(Info),

        io.format("--> ok: files: %i, README.md: %i, Mercury: %i\n",
            [i(file_count(Info)), i(readme_count(Info)),
            i(m_count(Info))], !IO)
    ;
        Res = io.error(_, Err),

        io.format("error: %s\n",
            [s(io.error_message(Err))], !IO)
    ).


:- type info
    --->    info(
                display_file :: bool,
                file_count   :: int,
                readme_count :: int,
                m_count      :: int
            ).

%----------------------------------------------------------------------------%
:- end_module sample.
%----------------------------------------------------------------------------%
