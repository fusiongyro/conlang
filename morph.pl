:- module(morph).

optional([], [])         --> [].
optional([O|Os], [O|Rs]) --> O, optional(Os, Rs).
optional([_|Os], Rs)     --> optional(Os, Rs).

root(burz) --> "burz".
root(durb) --> "durb".
root(gimb) --> "gimb".

infinitive   --> "at".
third_person --> "ul".
plural       --> "um".

verb(Verb, Mods) --> 
    root(Verb), 
    optional([infinitive, third_person, plural], Mods).

