# Prolog CFG
A parser for a very simple subset of English. It is intentionally overly
accepting and tries its best to get a parse tree for you.

If it doesn't know a word, it will ask you to supply the word's part of speech.
It then learns the word so that next time you run the parser, it remembers what
it learned.

## References
This grammar was based heavily on the one developed by Michael A. Covington in [Natural Language Processing for Prolog Programmers](http://www.covingtoninnovations.com/books/NLPPP.pdf).

## Examples

```prolog
?- use_module(repl).
true.

?- repl.
[DB attached]
Sentence: she looked up the tower
1. s(np(pn(she)),vp(v(looked),pp(p(up),np(d(the),n1(n(tower))))))
2. s(np(pn(she)),vp(v(looked),part(up),np(d(the),n1(n(tower)))))

Sentence: the students challenged the quite unexpected statement at the conference that birds fly
1. s(np(d(the),n1(n(students))),vp(v(challenged),np(d(the),n1(adjp(deg(quite),adj(unexpected)),n1(n(statement))),pp(p(at),np(d(the),n1(n(conference)),s1(comp(that),s(np(d,n1(n(birds))),vp(v(fly)))))))))
2. s(np(d(the),n1(n(students))),vp(v(challenged),np(d(the),n1(adjp(deg(quite),adj(unexpected)),n1(n(statement))),pp(p(at),np(d(the),n1(n(conference)))),s1(comp(that),s(np(d,n1(n(birds))),vp(v(fly)))))))
3. s(np(d(the),n1(n(students))),vp(v(challenged),np(d(the),n1(adjp(deg(quite),adj(unexpected)),n1(n(statement)))),pp(p(at),np(d(the),n1(n(conference)),s1(comp(that),s(np(d,n1(n(birds))),vp(v(fly))))))))
4. s(np(d(the),n1(n(students))),vp(v(challenged),np(d(the),n1(adjp(deg(quite),adj(unexpected)),n1(n(statement))),pp(p(at),np(d(the),n1(n(conference))))),s1(comp(that),s(np(d,n1(n(birds))),vp(v(fly))))))
5. s(np(d(the),n1(n(students))),vp(v(challenged),np(d(the),n1(adjp(deg(quite),adj(unexpected)),n1(n(statement)))),pp(p(at),np(d(the),n1(n(conference)))),s1(comp(that),s(np(d,n1(n(birds))),vp(v(fly))))))

Sentence: harold slept soundly
These words are unknown: [harold,soundly]
-  harold
    Part-of-speech tags: [determiner,degree,adjective,noun,preposition,pronoun,name,complementizer,verb,particle,adverb,conjunction]
    Enter part of speech (followed by '.'): name.
-  soundly
    Part-of-speech tags: [determiner,degree,adjective,noun,preposition,pronoun,name,complementizer,verb,particle,adverb,conjunction]
    Enter part of speech (followed by '.'): |: adverb.
1. s(np(nm(harold)),vp(v(slept),advp(adv(soundly))))

Sentence: Could not parse
Sentence: quit
Quitting...
[DB detached]
false.
```

## Project Organization

The grammar is specified in [syntax.pl](https://github.com/eignnx/prolog-cfg/blob/main/syntax.pl).

The `++>` operator is defined in [plus_plus_arrow.pl](https://github.com/eignnx/prolog-cfg/blob/main/plus_plus_arrow.pl).

The database of known words is stored in [_data.pl](https://github.com/eignnx/prolog-cfg/blob/main/_data.pl),
and its public interface is defined in [data.pl](https://github.com/eignnx/prolog-cfg/blob/main/data.pl).
