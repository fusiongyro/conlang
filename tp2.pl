%% This is a second attempt at handling Toki Pona from Prolog, this
%% time going from the pu, the Toki Pona book.

%% Lesson 1 content

word(jelo).
word(jaki).
word(ken).
word(mu).
word(mun).
word(open).
word(mani).
word(wan).

%% Lesson 2 content and parser
tpnoun(ijo, thing, things).
tpnoun(jan, person, people).
tpnoun(kili, fruit, fruits).
tpnoun(lipu, document, documents).
tpnoun(meli, woman, women).
tpnoun(soweli, animal, animals).

noun(Noun) --> [Noun], { tpnoun(Noun, _, _) }.
noun(ni) --> [ni].
li --> [li].

sentence(X = Y) --> noun(X), li, noun(Y).

english(X=Y, Translation) :- phrase(Translation, tp2english(X=Y)).

tp2english(X=Y) --> enp(X, sing), [is], enp(Y, sing).
tp2english(X=Y) --> enp(X, pl), [are], enp(Y, pl).
enoun(X, sing) --> { tpnoun(X, Sing, _) }, [Sing].
enoun(X, pl) --> { tpnoun(X, _, Pl) }, [Pl].

enp(X, Num) --> det(Num), enoun(X, Num).
enp(ni, sing) --> [this].
enp(ni, pl) --> [these].

det(_) --> [the].
det(sing) --> [a].
det(pl) --> [].


toki_english(Toki, English) :-
    phrase(sentence(X), Toki),
    phrase(tp2english(X), English).

%% example usage:
%% toki_english([lipu,li,ijo], English).
%% toki_english(Toki, [the,document,is,a,thing]).


:- begin_tests(ch2).

test(tp2e_the_a, [nondet]) :-
    toki_english([lipu,li,ijo], English),
    English = [the,document,is,a,thing].
test(tp2e_the_the, [nondet]) :-
    toki_english([lipu,li,ijo], English),
    English = [the,document,is,the,thing].
test(tp2e_the_the_pl, [nondet]) :-
    toki_english([lipu,li,ijo], English),
    English = [the,documents,are,the,things].
test(e2tp_the_a, [nondet]) :-
    toki_english(Toki, [the,document,is,a,thing]),
    Toki = [lipu,li,ijo].
test(e2tp_the_the, [nondet]) :-
    toki_english(Toki, [the,document,is,the,thing]),
    Toki = [lipu,li,ijo].
test(e2tp_the_the_pl, [nondet]) :-
    toki_english(Toki, [the,documents,are,the,things]),
    Toki = [lipu,li,ijo].

test(tp2e_this, [nondet]) :-
    toki_english([ni,li,lipu], English),
    English = [this,is,the,document].
test(tp2e_these, [nondet]) :-
    toki_english([ni,li,lipu], English),
    English = [these,are,the,documents].
test(e2tp_this, [nondet]) :-
    toki_english(Toki, [this,is,the,document]),
    Toki = [ni,li,lipu].
test(e2tp_these, [nondet]) :-
    toki_english(Toki, [these,are,the,documents]),
    Toki = [ni,li,lipu].

:- end_tests(ch2).

