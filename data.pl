:- module(data, [
    unknown_word/1,
    learn_unknown_words/1,
    d_/1,
    deg_/1,
    adj_/1,
    n_/1,
    p_/1,
    pn_/1,
    nm_/1,
    comp_/1,
    v_/1,
    part_/1,
    adv_/1,
    conj_/1
]).

unknown_word(Word) :-
    nonvar(Word),
    !,
    \+ d_(Word),
    \+ deg_(Word),
    \+ adj_(Word),
    \+ n_(Word),
    \+ p_(Word),
    \+ pn_(Word),
    \+ nm_(Word),
    \+ comp_(Word),
    \+ v_(Word),
    \+ part_(Word),
    \+ adv_(Word),
    \+ conj_(Word),
    !.

unknown_word(X) :-
    var(X),
    !,
    throw(instantiation_error(X)).

% learn_unknown_words(+Words)
%  For each word that is not defined in this module, it will
%  ask the user to categorize the word, then it will learn
%  that categorization.

learn_unknown_words(Words) :-
    include(unknown_word, Words, UnknownWords),
    write('These words are unknown: '),
    write_canonical(UnknownWords), nl,
    learn_unknown_words_rec(UnknownWords).

learn_unknown_words([]).

learn_unknown_words_rec([]).

learn_unknown_words_rec([Word | Words]) :-
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
        v_,
        part_,
        adv_,
        conj_
    ],
    tab(4), write('Part-of-speech tags: '), write_canonical(Pos), nl,
    tab(4), write('Enter part of speech (followed by ''.''): '),
    read(PartOfSpeech),
    Fact =.. [PartOfSpeech, Word],
    learn_fact(Fact),
    learn_unknown_words_rec(Words).

learn_fact(Fact) :-
    assertz(Fact), % Save it to working memory.
    open('data.pl', append, Stream),
    write_term(Stream, Fact, [nl, fullstop]),
    close(Stream).

% Determiners
:- dynamic d_/1.
:- discontiguous d_/1.
d_(my).
d_(your).
d_(his).
d_(her).
d_(their).
d_(our).
d_(the).
d_(a).
d_(an).
d_(every).
d_(all).
d_(no).
d_(some).
d_(one).
d_(two).
d_(three).

% Degrees
:- dynamic deg_/1.
:- discontiguous deg_/1.
deg_(very).
deg_(surprisingly).
deg_(slightly).
deg_(extremely).
deg_(really).
deg_(quite).
deg_(kinda).
deg_(annoyingly).

% Adjectives
:- dynamic adj_/1.
:- discontiguous adj_/1.
adj_(good).
adj_(grey).
adj_(young).
adj_(big).
adj_(green).
adj_(hairy).
adj_(fat).
adj_(noisy).
adj_(unexpected).
adj_(amazing).

% Nouns
:- dynamic n_/1.
:- discontiguous n_/1.
n_(boy).
n_(girl).
n_(dog).
n_(cat).
n_(telescope).
n_(garden).
n_(conference).
n_(birds).
n_(statement).
n_(students).
n_(fact).

% Prepositions
:- dynamic p_/1.
:- discontiguous p_/1.
p_(P) :- part_(P). % All particles are prepositions.
p_(into).
p_(in).
p_(from).
p_(at).
p_(to).
p_(with).
p_(near).
p_(behind).
p_(around).

% Pronouns
:- dynamic pn_/1.
:- discontiguous pn_/1.
pn_(i).
pn_(me).
pn_(you).
pn_(he).
pn_(him).
pn_(she).
pn_(her).
pn_(they).
pn_(them).
pn_(it).
pn_(we).
pn_(yall).
pn_(everyone).
pn_(everybody).

% Names
:- dynamic nm_/1.
:- discontiguous nm_/1.
nm_(jack).
nm_(jill).
nm_(mary).
nm_(felix).
nm_(max).
nm_(fido).

% Complementizers
:- dynamic comp_/1.
:- discontiguous comp_/1.
comp_(that).

% Verbs
:- dynamic v_/1.
:- discontiguous v_/1.
v_(fly).
v_(believed).
v_(surprised).
v_(challenged).
v_(seemed).
v_(is).
v_(was).
v_(leave).
v_(gave).
v_(chased).
v_(slept).
v_(said).
v_(revealed).

% Particles
:- dynamic part_/1.
:- discontiguous part_/1.
part_(out). % 'He threw out the trash'
part_(up). % 'She looked up the word'


% Adverbs
:- dynamic adv_/1.
:- discontiguous adv_/1.
adv_(both).
adv_(either).
adv_(neither).

% Conjunctions
:- dynamic conj_/1.
:- discontiguous conj_/1.
conj_(and).
conj_(or).
conj_(nor).

%%%%%%%%%%%%% HERE BE RANDOM LEARNED DEFINITIONS %%%%%%%%%%%%%%%%%%
% Concern: if I ever need to update all occurrances of one
% of these facts, its gonna be trickier to see where they
% all live cause these are all jumbled on top of each
% other. :/ Oh well. Just use 'find and replace'.

v_(passed).
n_(car).
v_(walked).
p_(past).
v_(went).
n_(store).
v_(looked).
adj_(silly).
nm_(john).
n_(tower).
nm_(joe).
v_(threw).
n_(trash).
nm_(dante).
pn_(my).
adj_(favorite).
n_(pet).
adj_(warm).
adj_(fuzzy).
adj_(dry).
