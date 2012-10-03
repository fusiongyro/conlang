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


%% phonotactics

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


%% grammar
%% taken from http://en.wikipedia.org/wiki/Toki_Pona#Syntax

sentence --> interjection.
sentence --> optional_subclause, optional_vocative, subject, predicate.
sentence --> optional_subclause, vocative, predicate.

interjection(a). 
interjection(ala). 
interjection(ike). 
interjection(jaki). 
interjection(mu). 
interjection(o). 
interjection(pakala). 
interjection(pona). 
interjection(toki).

interjection --> [A], { interjection(A) }.

optional_subclause --> [].
optional_subclause --> subclause.

subclause --> [taso], sentence, [la].
subclause --> [taso], noun, phrase, [la].

optional_vocative --> [].
optional_vocative --> noun_phrase, [o].

subject --> [mi].
subject --> [sina].
subject --> noun_phrase, [li].

predicate --> simple_noun_phrase, multiple_optional_prepositional_phrases.
predicate --> verb_phrase, optional_prepositional_phrase.
predicate --> predicate, conjuction, predicate.

noun_phrase --> noun, multiple_optional_modifiers.
noun_phrase --> simple_noun_phrase, [pi], noun_phrase.
noun_phrase --> noun_phrase, conjunction, noun_phrase.

multiple_optional_modifiers --> [].
multiple_optional_modifiers --> modifier, multiple_optional_modifiers.

conjunction --> [anu].
conjunction --> [en].

multiple_optional_prepositional_phrases --> [].
multiple_optional_prepositional_phrases --> prepositional_phrase, multiple_optional_prepositional_phrases.

prepositional_phrase --> preposition, noun_phrase.

verb_phrase --> verbal.
verb_phrase --> modal, verbal.
verb_phrase --> verbal, [ala], verbal.
verb_phrase --> modal, [ala], modal, verbal.

modal --> [kama].
modal --> [ken].
modal --> [wile].

verbal --> verb, multiple_optional_modifiers.
verbal --> verb, multiple_optional_modifiers, direct_object.
verbal --> [lon], simple_noun_phrase.
verbal --> [tawa], simple_noun_phrase.

direct_object --> [e], simple_noun_phrase.
