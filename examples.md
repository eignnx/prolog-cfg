# Cool Examples

```prolog
?- phrase(s(Parse), [the,students,challenged,the,quite,unexpected,statement,at,the,conference,that,birds,fly]).

s(np(d(the),n1(n(students))),vp(v(challenged),np(d(the),n1(adjp(deg(quite),adj(unexpected)),n1(n(statement))),
    pp(p(at),np(d(the),n1(n(conference)),s1(comp(that),s(np(d('0'),n1(n(birds))),vp(v(fly))))))
))).

s(np(d(the),n1(n(students))),vp(v(challenged),np(d(the),n1(adjp(deg(quite),adj(unexpected)),n1(n(statement))),pp(p(at),np(d(the),n1(n(conference)))),s1(comp(that),s(np(d('0'),n1(n(birds))),vp(v(fly))))))).
```