:- module(syntax, [s//1, s1//1, d//1, np//1, vp//1, pp//1]).
:- use_module(data).
:- use_module(plus_plus_arrow).

np ++> d, n1, pp, s1.
np ++> d, n1, pp.
np ++> d, n1, s1.
np ++> d, n1.
np ++> pn.
np ++> nm.
np ++> [both], np, [and], np.

d ++> [X], { data:word_type(X, determiner) }, !.
d ++> [], !.

% Multiple adjective-phrases followed by a noun.
n1 ++> n.
n1 ++> adjp, n1.

% Adjective Phrase - like 'very big', 'surprisingly big', ...
adjp ++> deg, adj.
adjp ++> adj.
adjp ++> [both], adjp, [and], adjp.

deg ++> [Degree], { data:word_type(Degree, degree) }.

adj ++> [X], { data:word_type(X, adjective) }.

n ++> [X], { data:word_type(X, noun) }.

% '[in] [the garden]'
pp ++> p, np.
% '[after] [he left the conference]'
pp ++> p, s.

pn ++> [Pn], { data:word_type(Pn, pronoun) }.

p ++> [P], { data:word_type(P, preposition) }.

nm ++> [Nm], { data:word_type(Nm, name) }.

% These are for sentences embedded in noun phrases.
% Example - 'the fact [that he slept]'
s1 ++> comp, s.

% Complementizer - 'the fact [[that] he slept]'
comp ++> [C], { data:word_type(C, complementizer) }, !.
% 'She said [[] he slept]'
comp ++> [], !.

% '[very] [quickly]'
advp ++> deg, adv. % TODO: replace this `adv` with `advp`?
advp ++> adv.

adv ++> [A], { data:word_type(A, adverb) }.

% s --> (advp), (pp), np, (advp), vp
% 'quickly he ate'
s ++> advp, np, vp.
% 'quickly into the forest she ran'
s ++> advp, pp, np, vp.
% '[After he left] [the discussion] [continued]'
s ++> pp, np, vp.
% '[the dog] [quickly] [ran]'
s ++> np, advp, vp.
% '[the dog] [ran]'
s ++> np, vp.

vp ++> [both], vp, [and], vp.

% vp --> v, (np), (pp), (np), (pp), (s1).
% TODO: THIS IS NOT GOOD ENOUGH, AND IT'S NOT EVEN COVERED COMPLETELY.
vp ++> v, np, pp, np, pp, s1.
vp ++> v, np, pp, np, pp.
vp ++> v, np, pp, np, s1.
vp ++> v, np, pp, np.
vp ++> v, np, pp, s1.
vp ++> v, np, pp, advp.
vp ++> v, np, advp, pp.
vp ++> v, np, pp.
vp ++> v, pp, np, s1.
vp ++> v, np, s1.
% 'Max [said that all birds fly]'
vp ++> v, s1.
% 'Mary [looked the tower up]'
vp ++> v, np, part.
% 'Mary [looked up the tower]'
vp ++> v, part, np.
vp ++> v, np.
% 'I [walked past the car]'
vp ++> v, pp.
% 'I [woke up]'
vp ++> v, part.
% 'John [seemed very old]'
vp ++> v, adjp, pp.
vp ++> v, adjp.
% 'John [wanted to leave]'
vp ++> v, [to], vp.
% 'Jesus [wept]'
vp ++> v.

v ++> [Verb], { data:word_type(Verb, verb) }.

part ++> [Particle], { data:word_type(Particle, particle) }.
