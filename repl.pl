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
      include(data:unknown_word, Atoms, UnknownAtoms),
      handle_unknown_words(UnknownAtoms),
      fail % Continue the repl loop.
    ).

handle_unknown_words([]).
handle_unknown_words(Words) :-
    write('These words are unknown: '),
    write_canonical(Words), nl,
    handle_unknown_words_rec(Words).

handle_unknown_words_rec([]).
handle_unknown_words_rec([Word | Words]) :-
    write('-'), tab(2), write(Word), nl,
    Pos = [
        d_,
        deg_,
        adj_,
        n_,
        p_,
        pn_,
        nm_,
        comp_,
        v_
    ],
    tab(4), write('Part-of-speech tags: '), write_canonical(Pos), nl,
    tab(4), write('Enter part of speech (followed by ''.''): '),
    read(PartOfSpeech),
    Fact =.. [PartOfSpeech, Word],
    assertz(Fact),
    handle_unknown_words_rec(Words).