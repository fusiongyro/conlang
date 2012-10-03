:- module(tokipona).

syllable_structure("(C)V(N)").

consonant(m, nasal, labial).
consonant(n, nasal, coronal).
consonant(p, plosive, labial).
consonant(t, plosive, coronal).
consonant(k, plosive, dorsal).
consonant(s, fricative, coronal).
consonant(w, approximant, labial).
consonant(l, approximant, coronal).
consonant(j, approximant, dorsal).

vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).


consonant1(C) --> [C], { consonant(C, _, _) }.
vowel(V) --> [V], { vowel(V) }.
nasal(N) --> [N], { consonant1(N, nasal, _) }.

syllable(S) --> syllable1(Chars), { atom_chars(S, Chars) }.

syllable1([C|S]) --> consonant1(C), syllable_body(S).
syllable1(S) --> syllable_body(S).

syllable_body([V,N]) --> vowel(V), nasal(N).
syllable_body([V]) --> vowel(V).

word(Word) --> syllable(Word).
word(Word) --> syllable(W), word(W1), { atom_concat(W, W1, Word) }.
