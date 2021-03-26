:- use_module(library(apply), [include/3]).
:- use_module(tokenize).
:- use_module(data).
:- use_module(syntax, [s//1]).
:- use_module(tests, [test_all/0]). % Allow testing from top-level.

% The main loop.
repl :-
    repeat,
        write('Sentence: '),
        tokenize:read_atomics(Atoms),
        show_then_quit_or_continue(Atoms),
        Atoms == [quit],
        write('Quitting...'), nl,
    !.

show_then_quit_or_continue(Atoms) :-
    % Important: `bagof` fails if phrase(..) fails.
    bagof(Tree, phrase(s(Tree), Atoms), Trees),
    !,
    show_solutions(Trees),
    fail. % Continue the repl loop.

show_then_quit_or_continue(Atoms) :-
    ( Atoms == [quit] -> true % Exit the repl loop.
    ; write('Could not parse.'), nl,
      data:learn_unknown_words(Atoms),
      fail % Continue the repl loop.
    ).

show_solutions([]).

show_solutions([Tree | Trees]) :-
    write_canonical(Tree), nl,
    show_solutions(Trees).
