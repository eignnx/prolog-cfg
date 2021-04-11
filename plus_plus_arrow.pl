:- module(plus_plus_arrow, [
    op(1200, xfx, ++>)
]).

:- op(1200, xfx, ++>).

/*
The rule:
s ++> np(blah), vp.
translates to:
s(s(Np, Vp)) --> np(blah, Np), vp(Vp).

n ++> [N], { noun(N) }.
expands to:
n(n([N])) --> [N], { noun(N) }.
*/

user:term_expansion((Head ++> Body), Expansion) :-
    plus_plus_arrow_expansion((Head ++> Body), Expansion).

plus_plus_arrow_expansion((Head0 ++> Body0), (Head --> Body)) :-
    comma_list(Body0, BodyList0),
    phrase(terminal_body_expansion(BodyList0, BodyList), Terminals),
    !,
    comma_list(Body, BodyList),
    head_payload_expansion(Head0, [Terminals], Head).
    
plus_plus_arrow_expansion((Head0 ++> Body0), (Head --> Body)) :-
    comma_list(Body0, BodyList0),
    phrase(nonterminal_body_expansion(BodyList0, BodyList), Vars),
    !,
    comma_list(Body, BodyList),
    head_payload_expansion(Head0, Vars, Head).

head_payload_expansion(Head0, Payload, Head) :-
    Head0 =.. [Functor|OriginalArgs],
    Syntax =.. [Functor|Payload],
    Head =.. [Functor,Syntax|OriginalArgs].

forbidden_functor(;).
forbidden_functor('|').

ignorable_functor({}).
ignorable_functor(!).

tag_terminal_symbol(X, [X]).

nonterminal_body_expansion([Term|Rest0], [Term|Rest]) -->
    {
        % If the head of the term is an ignorable functor,
        % pass it along unchanged.
        Term =.. [Head|_],
        ignorable_functor(Head),
        !
    },
    nonterminal_body_expansion(Rest0, Rest).

nonterminal_body_expansion([Term|_], _) -->
    {
        % If the head of the term is a forbidden functor,
        % throw an error, and halt translation.
        Term =.. [Head|_],
        forbidden_functor(Head),
        !,
        format(atom(Msg), 'Functor head `~a` is not allowed in the body of a `++>` rule!', [Head]),
        throw(error(plus_plus_arrow_expansion(Msg))),
        fail
    }.

nonterminal_body_expansion([Term|Rest0], [Term|Rest]) -->
    {
        is_list(Term),
        !
    },
    list([Term]), % Include the tokens in the syntax tree alongside subtrees from nonterminals.
    nonterminal_body_expansion(Rest0, Rest).

nonterminal_body_expansion([Nt0|Rest0], [Nt|Rest]) -->
    {
        Nt0 =.. [Functor|SuppliedArgs],
        Nt =.. [Functor, Var|SuppliedArgs]
    },
    [Var],
    nonterminal_body_expansion(Rest0, Rest).

nonterminal_body_expansion([], []) --> [].

%! terminal_body_expansion(Body0, Body)
%!
%! Expands `a ++> [x].` to `a(a([x])) --> [x].`
%! Expands `a ++> [].` to `a(a([])) --> [].`

terminal_body_expansion([L|Rest0], [L|Rest]) -->
    { is_list(L) },
    !,
    list(L), % We found the terminal symbol.
    terminal_body_expansion(Rest0, Rest).

terminal_body_expansion([Term|Rest0], [Term|Rest]) -->
    {
        Term =.. [Head|_],
        ignorable_functor(Head), % Pass it along unchanged.
        !
    },
    terminal_body_expansion(Rest0, Rest).

terminal_body_expansion([Term|_], _) -->
    {
        Term =.. [Head|_],
        forbidden_functor(Head),
        !,
        format(atom(Msg), 'Functor head `~a` is not allowed in the body of a `++>` rule!', [Head]),
        throw(error(plus_plus_arrow_expansion(Msg))),
        fail
    }.

terminal_body_expansion([], []) --> [].

is_list([]).
is_list([_|Xs]) :- is_list(Xs).

list([]) --> [].
list([X|Xs]) --> [X], list(Xs).