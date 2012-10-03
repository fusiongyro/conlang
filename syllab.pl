:- module(syllab).

stop(p).
approximant(l).
vowel(a).

consonant(C) --> [C], { stop(C) }.
consonant(C) --> [C], { approximant(C) }.
vowel(V) --> [V], { vowel(V) }.

syllable(S) --> syllable1(Chars), { atom_chars(S, Chars) }.

syllable1([O|Body]) --> onset(O), syllable_body(Body).
syllable1(Body) --> syllable_body(Body).

syllable_body([N|C]) --> nucleus(N), coda(C).
syllable_body([N]) --> nucleus(N).

onset(C) --> consonant(C), { C \= ng }.

nucleus(V) --> vowel(V).

coda([C]) --> consonant(C).
