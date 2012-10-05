:- module(morph).

verb(Verb, Mods) --> root(Verb), modifiers(Mods).

modifiers([]) --> [].
modifiers([infinitive|Rest]) --> "at", modifiers(Rest).
modifiers([third_person|Rest]) --> "ul", modifiers(Rest).
modifiers([plural|Rest]) --> "um", modifiers(Rest).

root(burz) --> "burz".
root(durb) --> "durb".
root(gimb) --> "gimb".

infinitive --> "at".
third_person --> "ul".
plural --> "um".

optional([], []) --> [].
optional([This|Results], [This|Rest]) --> This, !, optional(Results, [Rest]).
optional(Results, [_|Rest]) --> optional(Results, Rest).

verb2(Verb, Mods) --> root(Verb), optional([infinitive, third_person, plural], Mods), !.
