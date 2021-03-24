:- use_module(library(apply), [include/3]).
:- use_module(tokenize).
:- use_module(data).
:- use_module(syntax, [s//1]).

% The main loop.
repl :-
    repeat,
        write('Sentence: '),
        tokenize:read_atomics(Atoms),
        show_then_quit_or_continue(Tree, Atoms, phrase(s(Tree), Atoms)),
        Atoms == [quit],
        write('Quitting...'), nl,
    !.

show_then_quit_or_continue(Tree, _Atoms, Term) :-
    call(Term),
    !,
    write_canonical(Tree), nl,
    fail. % Continue the repl loop.

show_then_quit_or_continue(_Tree, Atoms, _Term) :-
    ( Atoms == [quit] -> true
    ; write('Could not parse.'), nl,
      data:learn_unknown_words(Atoms),
      fail % Continue the repl loop.
    ).
