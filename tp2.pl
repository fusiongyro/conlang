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
tpnoun(ni, this, these).
tpnoun(soweli, animal, animals).

noun(Noun) --> [Noun], { tpnoun(Noun, _, _) }.
li --> [li].

sentence(X = Y) --> noun(X), li, noun(Y).

english(X=Y, Translation) :- phrase(Translation, tp2english(X=Y)).

tp2english(X=Y) --> enp(X, sing), [is], enp(Y, sing).
tp2english(X=Y) --> enp(X, pl), [are], enp(Y, pl).
enoun(X, sing) --> { tpnoun(X, Sing, _) }, [Sing].
enoun(X, pl) --> { tpnoun(X, _, Pl) }, [Pl].

enp(X, Num) --> det(Num), enoun(X, Num).

det(_) --> [the].
det(sing) --> [a].
det(pl) --> [].


toki_english(Toki, English) :-
    phrase(sentence(X), Toki),
    phrase(tp2english(X), English).
