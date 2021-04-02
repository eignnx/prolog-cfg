:- module(data, [
    unknown_word/1,
    learn_unknown_words/1,
    init_db/0,
    close_db/0,
    word_type/2
]).
:- use_module(library(persistency)).

%! word_type(?Word, ?Type) is semidet.
% External interface to the `data_word_type` persistent predicate.

word_type(Word, Type) :-
    data_word_type(Word, Type).

% Allow subclassifications.
word_type(Word, Super) :-
    is_a(Sub, Super),
    data_word_type(Word, Sub).

% Every particle is a preposition.
is_a(particle, preposition).

unknown_word(Word) :-
    \+ word_type(Word, _).

%! learn_unknown_words(+Words).
%  For each word that is not defined in this module, it will
%  ask the user to categorize the word, then it will learn
%  that categorization.

learn_unknown_words(Words) :-
    include(unknown_word, Words, UnknownWords),
    dif(UnknownWords, []) ->
        write('These words are unknown: '), write_canonical(UnknownWords), nl,
        learn_words(UnknownWords)
    ; true.

learn_words([]).

learn_words([Word | Words]) :-
    write('-'), tab(2), write(Word), nl,
    bagof(Type, part_of_speech(Type), Pos),
    tab(4), write('Part-of-speech tags: '), write_canonical(Pos), nl,
    get_part_of_speech(Type),
    assert_data_word_type(Word, Type),
    learn_words(Words).

get_part_of_speech(Type) :-
    tab(4), write('Enter part of speech (followed by ''.''): '),
    read(Input),
    part_of_speech(Input) ->
        Type = Input
    ;
        tab(4), writeln('That''s not a valid part of speech!'),
        get_part_of_speech(Type).

db_file_name('_data.pl').

init_db :-
    db_file_name(DbFile),
    db_attach(DbFile, []),
    writeln('[DB attached]').

close_db :-
    db_detach,
    writeln('[DB detached]').
    
part_of_speech(determiner).
part_of_speech(degree).
part_of_speech(adjective).
part_of_speech(noun).
part_of_speech(preposition).
part_of_speech(pronoun).
part_of_speech(name).
part_of_speech(complementizer).
part_of_speech(verb).
part_of_speech(particle).
part_of_speech(adverb).
part_of_speech(conjunction).

% Set up a peristent predicate called `data_word_type`.
% The `persistent` directive defines the following
% additional predicates:
%   * data_word_type/2
%   * assert_data_word_type/2
%   * retract_data_word_type/2
%   * retractall_data_word_type/2

:- persistent
    data_word_type(word:atom, type:oneof([
        determiner,
        degree,
        adjective,
        noun,
        preposition,
        pronoun,
        name,
        complementizer,
        verb,
        particle,
        adverb,
        conjunction
    ])).
