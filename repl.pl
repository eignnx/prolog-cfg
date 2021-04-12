:- module(repl, [repl/0, test_all/0]).
:- use_module(library(apply), [include/3]).
:- use_module(tokenize).
:- use_module(data).
:- use_module(syntax, [s//1]).
:- use_module(tests, [test_all/0]). % Allow testing from top-level.

% The main loop.
repl :-
    setup_call_cleanup(
        data:init_db,
        loop,
        data:close_db
    ).

loop :-
    write('Sentence: '),
    tokenize:read_atomics(Atoms),
    process_input(Atoms).

process_input([quit]) :-
    !,
    writeln('Quitting...'),
    fail.

process_input(Atoms) :-
    % If there are any unknown words, learn them first.
    data:learn_unknown_words(Atoms),
    % Important: `setof` fails if phrase(..) fails.
    (  setof(Tree, phrase(s(Tree), Atoms), Trees)
    -> show_solutions(Trees)
    ;  writeln('Could not parse')
    ),
    loop.


show_solutions(Trees) :-
    show_solutions(1, Trees).

show_solutions(_, []) :- nl.

show_solutions(N, [Tree | Trees]) :-
    format('~d. ~k~n', [N, Tree]),
    N1 is N + 1,
    show_solutions(N1, Trees).
