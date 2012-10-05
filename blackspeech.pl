:- module(blackspeech).

% Ash nazg durbatulûk, ash nazg gimbatul,
% ash nazg thrakatulûk agh burzum-ishi krimpatul.

speech([ash, nazg, durbatulûk, 
        ash, nazg, gimbatul, 
        ash, nazg, thrakatulûk, agh, burzumishi, krimpatul]).

% lexicon
conjunction(agh, and).
preposition(ishi, in).

root(ash, one).
root(lug, tower).
root(nazg, ring).
root(burz, dark).
root(durb, rule).
root(gimb, find).
root(krimp, bind).
root(thrak, bring).

%% combinators
optional([], [])         --> [].
optional([O|Os], [O|Rs]) --> O, optional(Os, Rs).
optional([_|Os], Rs)     --> optional(Os, Rs).

%% noun morphology
% ishi  in

%% verb morphology
% at  infinitive
% ul  3p
% ûk  pl

utterance([Conj|Rest]) --> conjunction(Conj), utterance(Rest).
utterance([VP|Rest]) --> verb_phrase(VP), utterance(Rest).
utterance([]) --> [].

conjunction(conj(Conj, Left, Right)) --> verb_phrase(Left), [Conj], { conjunction(Conj, _) }, verb_phrase(Right).

infinitive --> "at".
person(3)  --> "ul".
number(pl) --> "ûk".

nominalize --> "um".
prep(ishi) --> "ishi".

noun_phrase(np(modifier(Modifier), Noun)) --> noun(Modifier), noun_phrase(Noun).
noun_phrase(np(Noun)) --> noun(Noun).

noun(noun(nominalized(root(Root)))) --> 
  [Noun], 
  {
    atom_concat(Root, Tail, Noun),
    phrase(root(Root), [Root]),
    atom_codes(Tail, ModifierCodes),
    phrase(nominalize, ModifierCodes)
  }. 
noun(noun(root(Noun)))        --> root(Noun).

prepositional_phrase(pp(Prep, NP)) -->
  [Noun],
  {
    atom_concat(Root, Tail, Noun),
    phrase(noun_phrase(NP), [Root]),
    atom_codes(Tail, ModifierCodes),
    phrase(prep(Prep), ModifierCodes)
  }.

verb_phrase(vp(NP, VP)) --> noun_phrase(NP), verb_phrase(VP).
verb_phrase(vp(PP, VP)) --> prepositional_phrase(PP), verb_phrase(VP).
% this one is fairly nasty
verb_phrase(vp(verb(Root, Modifiers))) -->
  [Verb],
  {
    atom_concat(Root, Tail, Verb),
    root(Root, _),
    atom_codes(Tail, ModifierCodes),
    phrase(optional([infinitive, person(_), number(_)], Modifiers), ModifierCodes)
  }.

descriptor(descriptor(NP, VP)) --> noun_phrase(NP), verb_phrase(VP).

root(Noun) --> [Noun], { root(Noun, _) }.

%% English translation
english([X|Xs], Result) :- english(X, Next), english(Xs, Rest), append(Next, Rest, Result).
english([], []).

english(conj(agh, Left, Right), English) :-
  english(Left, LeftEnglish),
  english(Right, RightEnglish),
  append(LeftEnglish, [and], HalfEnglish),
  append(HalfEnglish, RightEnglish, English).
  
english(descriptor(NP, VP), English) :-
  english(NP, EnglishNP),
  english(VP, EnglishVP),
  append(EnglishNP, EnglishVP, English).

english(vp(VP), English) :- english(VP, English).
english(vp(np(NP), VP), English) :- 
  english(VP, VerbEnglish),
  english(np(NP), NounEnglish), 
  append(NounEnglish, VerbEnglish, English).
english(vp(pp(Prep, PP), VP), English) :- 
  english(VP, VerbEnglish),
  english(pp(Prep, PP), PrepEnglish), 
  append(VerbEnglish, PrepEnglish, English).

english(verb(Root, [infinitive|Mods]), [to|Rest]) :- 
  english(verb(Root, Mods), Rest).
english(verb(Root, [person(3)|Mods]), Result) :- 
  english(verb(Root, Mods), Rest),
  append(Rest, [them], Result).
english(verb(Root, [number(pl)|Mods]), Result) :-
  english(verb(Root, Mods), Rest),
  append(Rest, [all], Result).
english(verb(Root, []), [English]) :- root(Root, English).

english(np(modifier(Mod), NP), Result) :- 
    english(Mod, EnglishMod),
    english(NP, EnglishPhrase),
    append(EnglishMod, EnglishPhrase, Result).
english(np(BS), English) :- english(BS, English).
english(root(X), [English]) :- root(X, English).

english(noun(root(X)), [Y]) :- root(X, Y).
english(pp(ishi, NP), [in|Rest]) :- english(NP, Rest).

% special cases
english(noun(nominalized(root(burz))), [darkness]).

display([Word|Rest]) :- write(Word), write(' '), display(Rest).
display([]) :- !.


main :-
  speech(RingSpeech),
  phrase(utterance(Tree), RingSpeech), 
  english(Tree, English),
  write('The ring speech: '),
  display(RingSpeech), nl,
  write('Parsed: '), nl,
  write(Tree), nl,
  write('Translated: '), nl,
  display(English), nl, !.