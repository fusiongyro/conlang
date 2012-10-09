:- module(blackspeech).

%% The One Ring speech:
%%
%% “Ash nazg durbatulûk, ash nazg gimbatul,
%% ash nazg thrakatulûk agh burzum-ishi krimpatul.”
speech([ash, nazg, durbatulûk, 
        ash, nazg, gimbatul, 
        ash, nazg, thrakatulûk, agh, burzumishi, krimpatul]).

%% This will be translated into the following English:
%% [one, ring, to, rule, them, all, 
%%  one, ring, to, find, them, 
%%  one, ring, to, bring, them, all, and, to, bind, them, in, darkness].

%% Many different parse trees are created. I suspect I have made my grammar
%% rules overly general. However, all of the translations come out
%% identically.

%% lexicon
root(ash, one).
root(lug, tower).
root(nazg, ring).
root(burz, dark).
root(durb, rule).
root(gimb, find).
root(krimp, bind).
root(thrak, bring).

conjunction(agh, and).
preposition(ishi, in).

%% generic grammatical combinators
optional([], [])         --> [].
optional([O|Os], [O|Rs]) --> O, optional(Os, Rs).
optional([_|Os], Rs)     --> optional(Os, Rs).

%% verb morphology
infinitive --> "at".
person(3)  --> "ul".
number(pl) --> "ûk".

%% noun morphology
nominalized(Root, Nominalized) :- root(Root, _), atom_concat(Root, um, Nominalized).
illative(Root, Illative) :- atom_concat(Root, ishi, Illative).

%% top production
utterance([Conj|Rest]) --> conjunction(Conj), utterance(Rest).
utterance([VP|Rest])   --> verb_phrase(VP), utterance(Rest).
utterance([])          --> [].

%% basic grammar
%% conjunctions
conjunction(conj(Conj, Left, Right)) --> verb_phrase(Left), [Conj], { conjunction(Conj, _) }, verb_phrase(Right).

%% noun phrases: 'modifier noun' or 'noun'
noun_phrase(np(modifier(Modifier), Noun)) --> noun(Modifier), noun_phrase(Noun).
noun_phrase(np(Noun)) --> noun(Noun).

%% nouns: root words or nominalized roots ("dark" -> "darkness")
noun(noun(nominalized(root(Root)))) --> [Noun], { nominalized(Root, Noun) }. 
noun(noun(root(Noun)))              --> root(Noun).

%% prepositional "phrases" (is it still a 'phrase' if it's one word inflected?)
prepositional_phrase(pp(ishi, Noun)) -->
  [Word], { illative(Root, Word), phrase(noun(Noun), [Root]) }.

%% verb phrases: 'noun verb' or 'prepositional-noun verb'
verb_phrase(vp(NP, VP)) --> noun_phrase(NP), verb_phrase(VP).
verb_phrase(vp(PP, VP)) --> prepositional_phrase(PP), verb_phrase(VP).

%% the root verb phrase: a root word plus a pile of optional verbal modifiers
verb_phrase(vp(verb(Root, Modifiers))) -->
  [Verb],
  {
    atom_concat(Root, Tail, Verb),
    root(Root, _),
    atom_codes(Tail, ModifierCodes),
    phrase(optional([infinitive, person(_), number(_)], Modifiers), ModifierCodes)
  }.

root(Noun) --> [Noun], { root(Noun, _) }.

%% English translation
english([X|Xs], Result) :- english(X, Next), english(Xs, Rest), append(Next, Rest, Result).
english([], []).

%% conj(agh, Left, Right) --> Left 'and' Right
english(conj(agh, Left, Right), English) :-
  english(Left, LeftEnglish),
  english(Right, RightEnglish),
  append(LeftEnglish, [and], HalfEnglish),
  append(HalfEnglish, RightEnglish, English).
  
%% descriptor(Noun, Verb) --> Noun, Verb
english(descriptor(NP, VP), English) :-
  english(NP, EnglishNP),
  english(VP, EnglishVP),
  append(EnglishNP, EnglishVP, English).

%% vp(Verb) --> Verb
english(vp(VP), English) :- english(VP, English).

%% vp(NounPhrase, VerbPhrase) --> NounPhrase, VerbPhrase
english(vp(np(NP), VP), English) :- 
  english(VP, VerbEnglish),
  english(np(NP), NounEnglish), 
  append(NounEnglish, VerbEnglish, English).

%% vp(PrepositionalPhrase, VerbPhrase) --> VerbPhrase, PrepositionalPhrase
english(vp(pp(Prep, PP), VP), English) :- 
  english(VP, VerbEnglish),
  english(pp(Prep, PP), PrepEnglish), 
  append(VerbEnglish, PrepEnglish, English).

%% verb(... [infinitive]) --> 'to', verb(...)
english(verb(Root, [infinitive|Mods]), [to|Rest]) :- english(verb(Root, Mods), Rest).

%% verb(... [3rd person plural]) --> verb(...) 'them' 'all' 
english(verb(Root, [person(3), number(pl)|Mods]), Result) :-
  english(verb(Root, Mods), Rest),
  append(Rest, [them, all], Result), !.
  
%% verb(... [3rd person]) --> verb(...) 'them'
english(verb(Root, [person(3)|Mods]), Result) :- 
  english(verb(Root, Mods), Rest),
  append(Rest, [them], Result).

%% verb(... [plural]) --> verb(...) 'all'
english(verb(Root, [number(pl)|Mods]), Result) :-
  english(verb(Root, Mods), Rest),
  append(Rest, [all], Result).

%% verb(...) --> root(...)
english(verb(Root, []), [English]) :- root(Root, English).

%% np(Adjective, Noun) --> Adjective, Noun
english(np(modifier(Mod), NP), Result) :- 
    english(Mod, EnglishMod),
    english(NP, EnglishPhrase),
    append(EnglishMod, EnglishPhrase, Result).

%% np(Noun) --> Noun
english(np(BS), English) :- english(BS, English).

%% root(X) --> english(X)
english(root(X), [English]) :- root(X, English).

%% noun(X) --> X
english(noun(root(X)), [Y]) :- root(X, Y).

%% pp(ishi, NP) --> 'in', NP
english(pp(ishi, NP), [in|Rest]) :- english(NP, Rest).

%% -- special cases --
%% burzum --> darkness
english(noun(nominalized(root(burz))), [darkness]).


%% helper
display([Word|Rest]) :- write(Word), write(' '), display(Rest).
display([]) :- !.


main :-
  speech(RingSpeech),
  phrase(utterance(Tree), RingSpeech), 
  english(Tree, English),
  write('The ring speech: '),
  display(RingSpeech), nl,
  write('Parsed: '), nl,
  portray_clause(Tree), nl,
  write('Translated: '), nl,
  display(English), nl, !.