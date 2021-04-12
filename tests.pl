:- module(tests, [test_all/0]).
:- use_module(syntax, [s//1]).
:- use_module(data).

test_all :-
    setup_call_cleanup(
        data:init_db,
        (
            findall(Tc, tests:test_case(Tc), TestCases),
            test_all_parse(TestCases, [])
        ),
        data:close_db
    ).

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
test_case([mary,looked,up,the,tower]). % TODO: ensure this parses two ways.
test_case([joe,threw,out,the,trash]).
test_case([joe,threw,the,trash,out]).
test_case([it,is,both,very,warm,and,quite,dry]).
test_case([john,drank,both,coffee,and,tea]).
test_case([i,both,ate,and,slept,in,the,tower]).
test_case([i,saw,him,after,he,left]).
test_case([the,discussion,was,surprising,after,he,left]).
test_case([the,discussion,after,he,left,was,surprising]).
test_case([after,he,left,the,discussion,was,surprising]).
test_case([quickly,he,chased,it,into,the,garden]).
test_case([he,quickly,chased,it,into,the,garden]).
test_case([he,chased,it,quickly,into,the,garden]).
test_case([he,chased,it,into,the,garden,quickly]).
test_case([jesus,wept]).
test_case([john,wanted,to,leave]).
test_case([john,seemed,very,old]).
test_case([john,seemed,very,old,in,his,own,way]).
test_case([i,woke,up]).
test_case([i,walked,past,the,car]).
test_case([quickly,he,ate]).