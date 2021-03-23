:- module(data, [
    unknown_word/1,
    d_/1,
    deg_/1,
    adj_/1,
    n_/1,
    p_/1,
    pn_/1,
    nm_/1,
    comp_/1,
    v_/1
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
    !.

unknown_word(X) :-
    var(X),
    !,
    throw(instantiation_error(X)).


% Determiners
:- dynamic d_/1.
d_(the).
d_(a).
d_(an).
d_(every).
d_(some).
d_(one).
d_(two).
d_(three).

% Degrees
:- dynamic deg_/1.
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
adj_(good).
adj_(grey).
adj_(young).
adj_(big).
adj_(green).
adj_(hairy).
adj_(fat).
adj_(noisy).
adj_(unexpected).

% Nouns
:- dynamic n_/1.
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

% Names
:- dynamic nm_/1.
nm_(jack).
nm_(jill).
nm_(mary).
nm_(felix).
nm_(max).
nm_(fido).

% Complementizers
:- dynamic comp_/1.
comp_(that).

% Verbs
:- dynamic v_/1.
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