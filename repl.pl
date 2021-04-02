:- module(repl, [repl/0]).
:- use_module(library(apply), [include/3]).
:- use_module(tokenize).
:- use_module(data).
:- use_module(syntax, [s//1]).
:- use_module(tests, [test_all/0]). % Allow testing from top-level.

% The main loop.
repl :-
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
    % Important: `bagof` fails if phrase(..) fails.
    (  bagof(Tree, phrase(s(Tree), Atoms), Trees)
    -> show_solutions(Trees)
    ;  writeln('Could not parse'), write_canonical(Atoms)
    ),
    repl.

show_solutions([]).

show_solutions([Tree | Trees]) :-
    write_canonical(Tree), nl,
    show_solutions(Trees).
