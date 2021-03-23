:- module(tokenize, [read_atomics/1]).


% read_atomics(-Atomics)
%  Reads a line of text, breaking it into a
%  list of atomic terms: [this,is,an,example].

read_atomics(Atomics) :-
    read_char(FirstChar, FirstCharType),
    complete_line(FirstChar, FirstCharType, Atomics).

% read_char(-Char, -CharType)
%  Reads a character and runs it through `char_type/3`.

read_char(Char, CharType) :-
    get_code(Code),
    char_type(Code, CharType, Char).

% complete_line(+FirstChar, +FirstCharType, -Atomics)
%  Given `FirstChar` (the first character) and `FirstCharType`
%  (its type), reads and tokenizes the rest of the line into
%  atoms and numbers.

complete_line(_, end, []) :- !. % Stop at end.

complete_line(_, blank, Atomics) :- % Skip blanks.
    !,
    read_atomics(Atomics).

complete_line(FirstChar, special, [A | Atomics]) :- % Special char.
    !,
    atom_chars(A, [FirstChar]),
    read_atomics(Atomics).

complete_line(FirstChar, alpha, [A | Atomics]) :- % Begin word.
    complete_word(FirstChar, alpha, Word, NextChar, NextCharType),
    atom_chars(A, Word),
    complete_line(NextChar, NextCharType, Atomics).

complete_line(FirstChar, numeric, [N | Atomics]) :- % Begin word.
    complete_word(FirstChar, numeric, Word, NextChar, NextCharType),
    catch(number_chars(N, Word),
          error(syntax_error(illegal_number), _),
          atom_chars(N, Word)),
    complete_line(NextChar, NextCharType, Atomics).

% complete_word(+FirstChar, +FirstCharType, -Word, -FollowingChar, -FollowingCharType)
%  Given `FirstChar` and `FirstCharType`, reads the rest of the word
%  putting its characters into `Word`.

complete_word(FirstChar, alpha, [FirstChar | Word], FollowingChar, FollowingCharType) :-
    !,
    read_char(NextChar, NextCharType),
    complete_word(NextChar, NextCharType, Word, FollowingChar, FollowingCharType).

complete_word(FirstChar, numeric, [FirstChar | Word], FollowingChar, FollowingCharType) :-
    !,
    read_char(NextChar, NextCharType),
    complete_word(NextChar, NextCharType, Word, FollowingChar, FollowingCharType).

complete_word(FirstChar, FirstCharType, [], FirstChar, FirstCharType).
    % Where `FirstCharType` is not `alpha` or `numeric`.

% char_type(+Code, ?Type, -Char)
%  Given an ASCII code, classifies the character as
%  `end` (of line/file), `blank`, `alpha`(numeric), or
%  `special`, and changes it to a potentially different
%  character (`Char`).

char_type(10, end, 10) :- !. % UNIX end of line mark.
char_type(13, end, 13) :- !. % DOS end of line mark.
char_type(-1, end, -1) :- !. % get0 end of file code.

char_type(Code, blank, 32) :- % Blanks and other control codes.
    Code =< 32,
    !.

char_type(Code, numeric, Code) :- % Digits.
    48 =< Code, Code =< 57,
    !.

char_type(Code, alpha, Code) :- % Lower-case letters.
    97 =< Code, Code =< 122,
    !.

char_type(Code, alpha, NewCode) :- % Upper-case letters.
    65 =< Code, Code =< 90,
    !,
    NewCode is Code + 32. % Translate to lower-case.

char_type(Code, special, Code). % All other codes are special characters.