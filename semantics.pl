s(Pred) --> np(Subj), vp(IndObj->Subj->Pred), pp(IndObj).
s(Pred) --> np(Subj), vp(Subj->Pred).

np(Name) -->
    [Name],
    { member(Name, [fido, felix, max, mary]) }.
np(the(Noun)) --> [The], { member(The, [the, that, this]) }, n(Noun).
np(a(Noun)) --> [A], { member(A, [a, an, some]) }, n(Noun).

n(Noun) --> [Noun], { noun(Noun) }.

noun(dog).
noun(cat).
noun(toy).
noun(stick).
noun(man).
noun(woman).
noun(child).
noun(garden).

% v class 1
v(Subj->slept(Subj)) --> [slept].
v(Subj->barked(Subj)) --> [barked].
v(Subj->walked(Subj)) --> [walked].

% v class 2
v(Obj->Subj->chased(Subj, Obj)) --> [chased].
v(Obj->Subj->saw(Subj, Obj)) --> [saw].
v(Obj->Subj->is(Subj, Obj)) --> [is].

% v class 3
v(IndObj->Obj->Subj->gave(Subj, Obj, IndObj)) --> [gave].
v(IndObj->Obj->Subj->sold(Subj, Obj, IndObj)) --> [sold].

% v class 4
v(Sent->Subj->said(Subj, Sent)) --> [said].
v(Sent->Subj->claimed(Subj, Sent)) --> [claimed].
v(Sent->Subj->thought(Subj, Sent)) --> [thought].

vp(Subj->Pred) --> v(Subj->Pred).
vp(Subj->Pred) --> v(Obj->Subj->Pred), np(Obj).
vp(Subj->Pred) --> v(IndObj->Obj->Subj->Pred), np(Obj), np(IndObj).
vp(Subj->Pred) --> v(Sent->Subj->Pred), s(Sent).

pp(into(N)) --> [into], np(N).
pp(around(N)) --> [around], np(N).
pp(with(N)) --> [with], np(N).
pp(for(N)) --> [for], np(N).
pp(IndObj) --> [to], np(IndObj).
