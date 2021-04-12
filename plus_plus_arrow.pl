:- module(plus_plus_arrow, [
    op(1200, xfx, ++>),
    op(1, fx, ?)
]).

:- use_module(library(clpfd)).

:- op(1, fx, ?).
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

user:term_expansion((Head0 ++> Body0), (Head --> Body)) :-
    % First expand the rule so that no more optionality syntax (`?blah`) exists.
    optional_expansions((Head0 ++> Body0), ExpandedRules),

    % Then translate `++>` rules to dcg `-->` rules.
    maplist(plus_plus_arrow_expansion, ExpandedRules, TranslatedRules),

    % Now join all these rules into one rule that shares a syntax tree variable.
    join_into_branching_syntax_rule(Head0, TranslatedRules, (Head --> Body)).

join_into_branching_syntax_rule(Head0, TranslatedRules, (Head --> Body)) :-
    maplist(body_to_final_assignment_form(SyntaxVar), TranslatedRules, FinalBodies),
    Head0 =.. [Functor|Args],
    Head =.. [Functor, SyntaxVar|Args],
    semicolon_list(Body, FinalBodies).

body_to_final_assignment_form(SyntaxVar, (Head0 --> Body0), Body) :-
    Head0 =.. [_Functor, SyntaxTree|_RemainingArgs],
    Body = (
        Body0,
        { SyntaxVar = SyntaxTree }
    ).

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

penetrable_functor(?).

tag_terminal_symbol(X, [X]).


% notion_expansion(+OldNotion, -TranslatedNotion)//.
%
% A `notion` is either a terminal or a nonterminal. It's just element that could
% appear on the right-hand side of a rule. A notion could also be a
% curly-brace-enclosed term.

notion_expansion(Notion, Notion) -->
    % If the head of the notion is an ignorable functor, pass it along
    % unchanged.
    {
        Notion =.. [Head|_],
        ignorable_functor(Head),
        !
    }.

notion_expansion(Notion0, Notion) -->
    % If the head of the term is a penetrable functor, translate its innards,
    % then wrap it back up in the penetrable functor.
    {
        Notion0 =.. [Head|Args],
        penetrable_functor(Head),
        !,
        [Inner] = Args % Ensure it only has one argument.
    },
    notion_expansion(Inner, ExpandedInner),
    {
        Notion =.. [Head, ExpandedInner] % Wrap it back up in the functor.
    }.

notion_expansion(Notion, Notion) -->
    % If the notion is a list of terminal symbols, expand them out into the list
    % of tracked values. We include the tokens in the syntax tree alongside
    % subtrees from nonterminals for readability's sake.
    {
        is_list(Notion),
        !
    },
    list([Notion]).

notion_expansion(Notion, _) -->
    % If the head of the term is a forbidden functor,
    % throw an error, and halt translation.
    {
        Notion =.. [Head|_],
        forbidden_functor(Head),
        !,
        format(atom(Msg), 'Functor head `~a` is not allowed in the body of a `++>` rule!', [Head]),
        throw(error(plus_plus_arrow_expansion(Msg))),
        fail
    }.

notion_expansion(Notion0, Notion) -->
    % In all other cases, it must be a functor. Add a variable to the list of
    % arguments, then track the notion.
    {
        Notion0 =.. [Functor|SuppliedArgs],
        Notion =.. [Functor, SyntaxVar|SuppliedArgs]
    },
    [SyntaxVar].

nonterminal_body_expansion([Notion0|Rest0], [Notion|Rest]) -->
    notion_expansion(Notion0, Notion),
    nonterminal_body_expansion(Rest0, Rest).

nonterminal_body_expansion([], []) --> [].

%! terminal_body_expansion(Body0, Body)
%
% Expands `a ++> [x].` to `a(a([x])) --> [x].`
% Expands `a ++> [].` to `a(a([])) --> [].`

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

% Accepts a rule with optionality syntax in its body and expands it into a list
% of rules, each with the same head, but with expanded bodies where an 'expanded'
% body E is one that is an 'optional subset' of the original body.

optional_expansions((Head ++> Body), Rules) :-
    comma_list(Body, BodyList),
    optional_powerset(BodyList, BodyLists),
    maplist(comma_list, Bodies, BodyLists),
    maplist({Head}/[B,R]>>((Head ++> B) = R), Bodies, Rules).

% [a, ?b, c] ~~> [[a, b, c], [a, c]]
% [?a, ?b] ~~> [[a, b], [a], [b], []]
optional_powerset(Superset, Powerset) :-
    bagof(S, optional_superset(Superset, S), Powerset).

optional_superset([], []).
optional_superset([?X|Rest0], [X|Rest]) :-
    optional_superset(Rest0, Rest).
optional_superset([?_|Rest0], Rest) :-
    optional_superset(Rest0, Rest).
optional_superset([X|Rest0], [X|Rest]) :-
    X \= ?_,
    !,
    optional_superset(Rest0, Rest).