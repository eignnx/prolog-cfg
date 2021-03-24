:- module(tests, [test_all/0]).
:- use_module(syntax, [s//1]).

test_all :-
    findall(Tc, tests:test_case(Tc), TestCases),
    test_all_parse(TestCases, []).

test_all_parse([], Failures) :-
    nl,
    show_failures(Failures).

test_all_parse([Input | Inputs], Failures) :-
    phrase(s(_Syntax), Input),
    !,
    write('.'),
    test_all_parse(Inputs, Failures).

test_all_parse([Input | Inputs], Failures) :-
    write('f'),
    test_all_parse(Inputs, [Input | Failures]).

show_failures([Failure | Failures]) :-
    format('Test Failed: ~k does not parse.', [Failure]), nl,
    data:learn_unknown_words(Failure),
    show_failures(Failures).

show_failures([]) :- nl.

test_case([max,revealed,to,everyone,the,amazing,fact,that,birds,fly]).
test_case([the,dog,slept]).
test_case([max,said,birds,fly]).
test_case([i,walked,past,the,car]).
test_case([i,passed,the,car]).
test_case([max,looked,very,silly]).