:- module(syntax, [s//1, s1//1, d//1, np//1, vp//1, pp//1]).
:- use_module(data).

np(np(Det, Noun1, Pp, S1)) --> d(Det), n1(Noun1), pp(Pp), s1(S1).
np(np(Det, Noun1, Pp)) --> d(Det), n1(Noun1), pp(Pp).
np(np(Det, Noun1, S1)) --> d(Det), n1(Noun1), s1(S1).
np(np(Det, Noun1)) --> d(Det), n1(Noun1).
np(np(Pn)) --> pn(Pn).
np(np(Nm)) --> nm(Nm).
np(np(both, Np1, conj(and), Np2)) --> [both], np(Np1), [and], np(Np2).

d(d(X)) --> [X], { data:d_(X) }, !.
d(d('0')) --> [], !.

% Multiple adjective-phrases followed by a noun.
n1(n1(N)) --> n(N).
n1(n1(AdjP, N1)) --> adjp(AdjP), n1(N1).

% Adjective Phrase - like 'very big', 'surprisingly big', ...
adjp(adjp(Degree, Adj)) --> deg(Degree), adj(Adj).
adjp(adjp(Adj)) --> adj(Adj).
adjp(adjp(both, Ap1, conj(and), Ap2)) --> [both], adjp(Ap1), [and], adjp(Ap2).

deg(deg(Degree)) --> [Degree], { data:deg_(Degree) }.

adj(adj(X)) --> [X], { data:adj_(X) }.

n(n(X)) --> [X], { data:n_(X) }.

pp(pp(p(P), Np)) --> { data:p_(P) }, [P], np(Np).

pn(pn(Pn)) --> [Pn], { data:pn_(Pn) }.

nm(nm(Nm)) --> [Nm], { data:nm_(Nm) }.

% These are for sentences embedded in noun phrases.
% Example - 'the fact [that he slept]'
s1(s1(C, Sentence)) --> comp(C), s(Sentence).

% Complementizer - 'the fact [[that] he slept]'
comp(comp(C)) --> [C], { data:comp_(C) }, !.
% 'She said [[] he slept]'
comp(comp('0')) --> [], !.

s(s(Np, Vp)) --> np(Np), vp(Vp).

vp(VpTree) --> v(V), vp_foll(V->VpTree).
vp(vp(both, Vp1, conj(and), Vp2)) --> [both], vp(Vp1), [and], vp(Vp2).

% vp --> v, (np), (pp), (np), (pp), (s1).
% TODO: THIS IS NOT GOOD ENOUGH, AND IT'S NOT EVEN COVERED COMPLETELY.
vp_foll(V->vp(V, Np1, Pp1, Np2, Pp2, S1)) --> np(Np1), pp(Pp1), np(Np2), pp(Pp2), s1(S1).
vp_foll(V->vp(V, Np1, Pp1, Np2, Pp2)) --> np(Np1), pp(Pp1), np(Np2), pp(Pp2).
vp_foll(V->vp(V, Np1, Pp1, Np2, S1)) --> np(Np1), pp(Pp1), np(Np2), s1(S1).
vp_foll(V->vp(V, Np1, Pp1, Np2)) --> np(Np1), pp(Pp1), np(Np2).
vp_foll(V->vp(V, Np1, Pp1, S1)) --> np(Np1), pp(Pp1), s1(S1).
vp_foll(V->vp(V, Np1, Pp1)) --> np(Np1), pp(Pp1).
vp_foll(V->vp(V, Pp, Np1, S1)) --> pp(Pp), np(Np1), s1(S1).
vp_foll(V->vp(V, Np1, S1)) --> np(Np1), s1(S1).
% 'Max said [that all birds fly]'
vp_foll(V->vp(V, S1)) --> s1(S1).
% 'Mary looked the tower up'
vp_foll(V->vp(V, Np, Particle)) --> np(Np), part(Particle).
% 'Mary looked up the tower'
vp_foll(V->vp(V, Particle, Np)) --> part(Particle), np(Np).
vp_foll(V->vp(V, Np)) --> np(Np).
% 'I walked [past the car]'
vp_foll(V->vp(V, Pp)) --> pp(Pp).
% 'John [seemed very old]'
vp_foll(V->vp(V, Adjp)) --> adjp(Adjp).
% 'John [wanted to leave]'
vp_foll(V->vp(V, Vp)) --> [to], vp(Vp).
% 'Jesus wept []'
vp_foll(V->vp(V)) --> [].

v(v(Verb)) --> [Verb], { data:v_(Verb) }.

part(part(Particle)) --> [Particle], { data:part_(Particle) }.
