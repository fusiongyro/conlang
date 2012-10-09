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

possible_word(W) --> syllable(W).
possible_word(W) --> syllable(Start), possible_word(Rest), { atom_concat(Start, Rest, W) }.

syllable(S) --> syllable1(Chars), { atom_chars(S, Chars) }.

syllable1([C|S]) --> consonant1(C), syllable_body(S).
syllable1(S) --> syllable_body(S).

syllable_body([V,N]) --> vowel(V), nasal(N).
syllable_body([V]) --> vowel(V).

wordgloss(Word) --> syllable(Word).
wordgloss(Word) --> syllable(W), wordgloss(W1), { atom_concat(W, W1, Word) }.


%% grammar
%% taken from http://en.wikipedia.org/wiki/Toki_Pona#Syntax

%% sample texts:
dark_teenage_poetry([
  [ijo, li, moku, e, mi],                               % Something is eating me.
  [mi, wile, pakala],                                   % I want to hurt.
  [pimeja, li, tawa, insa, kon, mi],                    % Darkness goes inside of me.
  [jan, ala, li, ken, sona, e, pilin, ike, mi],         % Nobody can know my pain.
  [toki, musi, o, sina, jan, pona, mi, wan, taso],      % Poetry, you are my one and only friend.
  [telo, pimeja, ni, li, telo, loje, mi, li, ale, mi],  % This ink is my blood, is my all.
  [tenpo, ale, la, pimeja, li, lon]                     % Darkness always exists.
]).

main :-
  dark_teenage_poetry(Lines),
  member(Line, Lines),
  show_parse(Line).

show_parse(X) :-
  write('Parsing: '), write(X), nl, 
  once(tokipona(X, Parsed)), !,
  write('Parsed: '),
  portray_clause(Parsed), nl.
show_parse(_) :-
  write('Unable to parse!'), nl.

tokipona(X, Y) :- phrase(sentence1(Y), X).

read_tokipona(Tree) :- 
  read_line_to_codes(user_input, Input),
  string_to_atom(Input,IA),
  atomic_list_concat(AlistI,' ',IA),
  tokipona(AlistI, Tree).

sentence1(interjection(I)) --> interjection(I).
sentence1(subclause(SC, S)) --> subclause(SC), simple_sentence(S).
sentence1(S) --> simple_sentence(S).

simple_sentence(vocative(Target, Predicate))      --> vocative(Target), predicate(Predicate).
simple_sentence(vocative_predicate(V, Subject, Predicate)) --> vocative(V), subject(Subject), predicate(Predicate).
simple_sentence(predicate(Subject, Predicate))    --> subject(Subject), predicate(Predicate).

%sentence(vocative(SC, Target, Predicate)) --> optional_subclause(SC), vocative(Target), predicate(Predicate).
%sentence(interjection(I))                 --> interjection(I).
%sentence(predicate(SC, V, S, P))          --> optional_subclause(SC), optional_vocative(V), subject(S), predicate(P).

interjection(a). 
interjection(ala). 
interjection(ike). 
interjection(jaki). 
interjection(mu). 
interjection(o). 
interjection(pakala). 
interjection(pona). 
interjection(toki).

interjection(I) --> [I], { interjection(I) }.

optional_subclause([]) --> [].
optional_subclause(SC) --> subclause(SC).

subclause(sentence(S))      --> optional_taso, simple_sentence(S), [la].
subclause(noun_phrase(NP))  --> optional_taso, noun_phrase(NP), [la].

optional_taso --> [taso].
optional_taso --> [].

optional_vocative([])  --> [].
optional_vocative([V]) --> vocative(V).

vocative(Target) --> noun_phrase(Target), [o].

subject(mi)               --> [mi].
subject(sina)             --> [sina].
subject(noun_phrase(NP))  --> noun_phrase(NP), [li].

predicate(conjunction(Conj, Left, Right)) --> simple_predicate(Left), conjunction(Conj), predicate(Right).
predicate(Predicate)                      --> simple_predicate(Predicate).

simple_predicate(noun_predicate(Noun, PPs)) --> noun_phrase(Noun), multiple_optional_prepositional_phrases(PPs).
simple_predicate(verb_predicate(Verb, PP))  --> verb_phrase(Verb), optional_prepositional_phrase(PP).

noun_phrase(pi(SimpleNoun, NounPhrase))   --> simple_noun_phrase(SimpleNoun), [pi], noun_phrase(NounPhrase).
noun_phrase(conj(C, LeftP, RightP))       --> simple_noun_phrase(LeftP), conjunction(C), noun_phrase(RightP).
noun_phrase(SimpleNoun)                   --> simple_noun_phrase(SimpleNoun).

simple_noun_phrase(noun(Noun, Modifiers)) --> noun(Noun), multiple_optional_modifiers(Modifiers).

multiple_optional_modifiers([])     --> [].
multiple_optional_modifiers([M|Ms]) --> modifier(M), multiple_optional_modifiers(Ms).

multiple_optional_prepositional_phrases([])       --> [].
multiple_optional_prepositional_phrases([PP|PPs]) --> prepositional_phrase(PP), multiple_optional_prepositional_phrases(PPs).

optional_prepositional_phrase([])   --> [].
optional_prepositional_phrase([PP]) --> prepositional_phrase(PP).

prepositional_phrase(pp(P, NP)) --> preposition(P), noun_phrase(NP).

verb_phrase(ala(Left, Right))             --> verbal(Left), [ala], verbal(Right).
verb_phrase(modal(ala(LeftM, RightM), V)) --> modal(LeftM), [ala], modal(RightM), verbal(V).
verb_phrase(modal(M, V))                  --> modal(M), verbal(V).
verb_phrase(V)                            --> verbal(V).

modal(kama) --> [kama].
modal(ken)  --> [ken].
modal(wile) --> [wile].

verbal(intransitive(V, Mod))    --> verb(V), multiple_optional_modifiers(Mod).
verbal(transitive(V, Mod, DO))  --> verb(V), multiple_optional_modifiers(Mod), direct_objects(DO).
verbal(lon(Noun))               --> [lon], simple_noun_phrase(Noun).
verbal(tawa(Noun))              --> [tawa], simple_noun_phrase(Noun).

direct_objects(conj(e, DO, DOs)) --> direct_object(DO), direct_objects(DOs).
direct_objects(DO) --> direct_object(DO).

direct_object(Noun) --> [e], noun_phrase(Noun).

% these are imprecise and should be addressed
verb(X) --> [X], { wordgloss(X, _) }.
noun(X) --> [X], { noun(X) }.
modifier(X) --> [X], { modifier(X) }.
preposition(P) --> [P], { preposition(P) }.

conjunction(X) --> [X], { conjunction(X) }.

%% conjunctions
conjunction(anu).   conjunction(en).

%% modifiers
modifier(akesi). 	modifier(laso).   	modifier(nasin). 	modifier(sin).
modifier(ala).   	modifier(lawa).   	modifier(nena).  	modifier(sinpin).
modifier(ale).   	modifier(len).    	modifier(ni).    	modifier(sitelen).
modifier(ali).   	modifier(lete).   	modifier(nimi).  	modifier(sona).
modifier(anpa).  	modifier(lili).   	modifier(noka).  	modifier(soweli).
modifier(ante).  	modifier(linja).  	modifier(oko).   	modifier(suli).
modifier(awen).  	modifier(lipu).   	modifier(olin).  	modifier(suno).
modifier(ijo).   	modifier(loje).   	modifier(open).  	modifier(supa).
modifier(ike).   	modifier(lon).    	modifier(pakala).	modifier(suwi).
modifier(insa).  	modifier(luka).   	modifier(pali).  	modifier(tan).
modifier(jaki).  	modifier(lukin).  	modifier(palisa).	modifier(taso).
modifier(jan).   	modifier(lupa).   	modifier(pan).   	modifier(tawa).
modifier(jelo).  	modifier(ma).     	modifier(pana).  	modifier(telo).
modifier(jo).    	modifier(mama).   	modifier(pilin). 	modifier(tenpo).
modifier(kala).  	modifier(mani).   	modifier(pimeja).	modifier(toki).
modifier(kalama).	modifier(meli).   	modifier(pini).  	modifier(tomo).
modifier(kama).  	modifier(mi).     	modifier(pipi).  	modifier(tu).
modifier(kasi).  	modifier(mije).   	modifier(poka).  	modifier(unpa).
modifier(kili).  	modifier(moku).   	modifier(poki).  	modifier(uta).
modifier(kin).   	modifier(moli).   	modifier(pona).  	modifier(utala).
modifier(kiwen). 	modifier(monsuta).	modifier(sama).  	modifier(walo).
modifier(ko).    	modifier(mun).    	modifier(seli).  	modifier(wan).
modifier(kon).   	modifier(musi).   	modifier(selo).  	modifier(waso).
modifier(kule).  	modifier(mute).   	modifier(sewi).  	modifier(wawa).
modifier(kulupu).	modifier(namako). 	modifier(sijelo).	modifier(weka).
modifier(kute).  	modifier(nasa).   	modifier(sike).  	modifier(wile).
modifier(lape).  	


%% prepositions
preposition(anpa).  preposition(poka).  preposition(sinpin).
preposition(insa).  preposition(selo).  preposition(tan).
preposition(jo).    preposition(sewi).  preposition(tawa).
preposition(lon).                       

%% nouns
noun(akesi).  	  noun(kulupu). 	  noun(nanpa).    noun(sike).
noun(ala).    	  noun(kute).   	  noun(nasa).     noun(sin).
noun(alasa).  	  noun(lape).   	  noun(nasin).    noun(sina).
noun(ale).    	  noun(laso).   	  noun(nena).     noun(sinpin).
noun(ali).    	  noun(lawa).   	  noun(ni).       noun(sitelen).
noun(anpa).   	  noun(len).    	  noun(nimi).     noun(sona).
noun(ante).   	  noun(lete).   	  noun(noka).     noun(soweli).
noun(awen).   	  noun(lili).   	  noun(oko).      noun(suli).
noun(esun).   	  noun(linja).  	  noun(olin).     noun(suno).
noun(ijo).    	  noun(lipu).   	  noun(ona).      noun(supa).
noun(ike).    	  noun(loje).   	  noun(open).     noun(suwi).
noun(ilo).    	  noun(luka).   	  noun(pakala).   noun(tan).
noun(insa).   	  noun(lukin).  	  noun(pali).     noun(taso).
noun(jaki).   	  noun(lupa).   	  noun(palisa).   noun(tawa).
noun(jan).    	  noun(ma).     	  noun(pan).      noun(telo).
noun(jelo).   	  noun(mama).   	  noun(pana).     noun(tenpo).
noun(jo).     	  noun(mani).   	  noun(pilin).    noun(toki).
noun(kala).   	  noun(meli).   	  noun(pimeja).   noun(tomo).
noun(kalama). 	  noun(mi).     	  noun(pini).     noun(tu).
noun(kama).   	  noun(mije).   	  noun(pipi).     noun(unpa).
noun(kasi).   	  noun(moku).   	  noun(poka).     noun(uta).
noun(ken).    	  noun(moli).   	  noun(poki).     noun(utala).
noun(kepeken).	  noun(monsi).  	  noun(pona).     noun(walo).
noun(kili).   	  noun(monsuta).	  noun(sama).     noun(wan).
noun(kipisi). 	  noun(mu).     	  noun(seli).     noun(waso).
noun(kiwen).  	  noun(mun).    	  noun(selo).     noun(wawa).
noun(ko).     	  noun(musi).   	  noun(seme).     noun(weka).
noun(kon).    	  noun(mute).   	  noun(sewi).     noun(wile).
noun(kule).   	  noun(namako). 	  noun(sijelo).   

%% wordgloss(word, gloss)
wordgloss(a, "ah!, ha!, uh!, oh!, ooh!, aw!, well!").
wordgloss(ala, "no, not, none, un-, opposite of, nothing, zero, no!").
wordgloss(anpa, "down, bottom, lower part, under, below, floor, beneath, low").
wordgloss(e, "separator, introduces the direct object").
wordgloss(anu, "or").
wordgloss(en, "and (joins together two nouns)").
wordgloss(ken, "can, is able to, is allowed to, may, possibility, to make possible, it is possible that").
wordgloss(akesi, "non-cute animal, reptile, amphibian, creeping animal, large arthropod").
wordgloss(alasa, "to gather, to hunt, to collect food, to gather, to harvest").
wordgloss(ale, "everything, anything, life, the universe, all, every, complete, whole").
wordgloss(ali, "everything, anything, life, the universe, all, every, complete, whole").
wordgloss(ante, "difference, different, otherwise, or else, to change, to alter, to modify").
wordgloss(awen, "to stay, to wait, to remain, stationary, permanent, sedentary").
wordgloss(esun, "market, shop").
wordgloss(ijo, "thing, something, stuff, anything, object, of something, to objectify").
wordgloss(ike, "bad, overly complex, evil, to worsen, to have a negative effect on, oh dear!, alas!").
wordgloss(ilo, "tool, device, machine, thing to be used for a specific purpose").
wordgloss(insa, "inside, inner world, center, stomach, internal").
wordgloss(jaki, "dirty, gross, filthy, dirt, pollution, garbage, filth, to pollute, to dirty, ew!, yuck!").
wordgloss(jan, "person, people, human, somebody, personal, to personify, to personalize").
wordgloss(jelo, "yellow, light green").
wordgloss(jo, "to have, to contain, having").
wordgloss(kala, "fish, sea creature").
wordgloss(kalama, "sound, noise, voice, make noise, to sound, to ring, to play (an instrument)").
wordgloss(kama, "to come, to become, to arrive, to bring about, to summon, event, future").
wordgloss(kasi, "plant, leaf, herb, tree, wood").
wordgloss(kepeken, "to use, using, with").
wordgloss(kili, "fruit, pulpy vegetable, a mushroom").
wordgloss(kin, "also, too, even, indeed (emphasizes the words before it)").
wordgloss(kipisi, "to cut").
wordgloss(kiwen, "hard, solid, stone-like, hard thing, rock, stone, metal, mineral, clay").
wordgloss(ko, "semi-solid or squishy substance").
wordgloss(kon, "air, wind, smell, soul, air-like, ethereal, gaseous").
wordgloss(kule, "color, paint, colorful, to color, to paint").
wordgloss(kulupu, "group, community, society, company, communal, shared, public, of the society").
wordgloss(kute, "to listen, to hear, auditory, hearing").
wordgloss(la, "separates adverb or phrase of context and sentence").
wordgloss(lape, "to sleep, to rest, sleeping").
wordgloss(laso, "blue, blue-green").
wordgloss(lawa, "head, mind, brain, main, leading, in charge, to lead, to control, to rule, to steer").
wordgloss(len, "clothes, cloth, fabric").
wordgloss(lete, "cold, uncooked, to cool down, to chill").
wordgloss(li, "separator, between any subject (except mi and sina) and its verb").
wordgloss(lili, "small, little, young, a bit, short, few, less, to reduce, to shorten, to shrink, to lessen").
wordgloss(linja, "string, rope, hair, thread, cord, chain").
wordgloss(lipu, "a flat bendable thing, paper, card, ticket").
wordgloss(loje, "red").
wordgloss(lon, "be (located) in, at, on, to exist, to be there, to be awake, to be present, to be real").
wordgloss(luka, "hand, arm").
wordgloss(lukin, "to see, to look at, to watch, to read, to pay attention, visual").
wordgloss(lupa, "hole, orifice, window, door").
wordgloss(ma, "earth, land, country, (outdoor) area").
wordgloss(mama, "parent, mother, father, parental").
wordgloss(mani, "money, material wealth, currency, capital").
wordgloss(meli, "woman, female, girl, wife, girlfriend, female, feminine, womanly").
wordgloss(mi, "I, me, we, my, our").
wordgloss(mije, "man, male, boy, husband, boyfriend, male, masculine, manly").
wordgloss(moku, "food, meal, to eat, to drink, to swallow, to ingest, to consume").
wordgloss(moli, "death, to die, to be dead, to kill, dead, deathly, fatal").
wordgloss(monsi, "back, rear end, behind, butt").
wordgloss(monsuta, "danger, predator, menace").
wordgloss(mu, "cute animal noise, woof!, meow!, moo!").
wordgloss(mun, "moon, lunar").
wordgloss(musi, "fun, playing, game, recreation, art, entertainment, to play, to amuse, to have fun").
wordgloss(mute, "many, very, much, several, a lot, amount, quantity, to make many or much").
wordgloss(namako, "extra, additional, spice").
wordgloss(nanpa, "number").
wordgloss(nasa, "crazy, silly, foolish, drunk, strange, stupid, weird, to drive crazy, to make weird").
wordgloss(nasin, "way, manner, custom, road, path, doctrine, method").
wordgloss(nena, "bump, extrusion, nose, hill, mountain, button").
wordgloss(ni, "that, this").
wordgloss(nimi, "word, a name").
wordgloss(noka, "leg, foot").
wordgloss(o, "calls someone's attention, hey!").
wordgloss(oko, "eye").
wordgloss(olin, "love, to love (a person)").
wordgloss(ona, "she, he, it, they, her, his, its, their").
wordgloss(open, "to open, to turn on").
wordgloss(pakala, "blunder, accident, destruction, to screw up, to damage, to break, damn!").
wordgloss(pali, "work, activity, deed, project, active, work-related, operating, to work, to act").
wordgloss(palisa, "rod, stick, branch").
wordgloss(pan, "grain, cereal").
wordgloss(pana, "to give, to put, to send, to place, to release, to cause, giving, transfer, exchange").
wordgloss(pi, "of, belonging to, used to separate a noun from another noun that has an adjective").
wordgloss(pilin, "feelings, emotion, heart, to feel, to think, to sense, to touch").
wordgloss(pimeja, "black, dark, darkness, shadows, to darken").
wordgloss(pini, "end, tip, completed, finished, past, done, ago, to finish, to close, to end, to tur").
wordgloss(pipi, "bug, spider, insect").
wordgloss(poka, "side, hip, next to, in the accompaniment of, with, neighboring").
wordgloss(poki, "box, container, cup, bowl, glass").
wordgloss(pona, "good, simplicity, simple, great!, to improve, to fix, to make good, beneficial").
wordgloss(sama, "same, similar, equal, like, as, seem").
wordgloss(seli, "fire, warmth, heat, hot, warm, cooked, to heat, to warm up, to cook").
wordgloss(selo, "outside, surface, skin, shell, bark, shape, peel").
wordgloss(seme, "what, which, wh- question").
wordgloss(sewi, "up, high, above, top, on, over, superior, elevated").
wordgloss(sijelo, "body, physical state").
wordgloss(sike, "circle, ball, wheel, sphere, cycle, round, cyclical").
wordgloss(sin, "new, fresh, another, more, to renew, to renovate, to freshen").
wordgloss(sina, "your, you").
wordgloss(sinpin, "front, chest, torso, face, wall").
wordgloss(sitelen, "picture, image, to draw, to write").
wordgloss(sona, "knowledge, wisdom, intelligence, understanding, to know, to understand").
wordgloss(soweli, "animal, land mammal, lovable animal").
wordgloss(suli, "big, tall, long, adult, important, to enlarge, to lengthen, size").
wordgloss(suno, "sun, light, shiny").
wordgloss(supa, "horizontal surface, supporting platform").
wordgloss(suwi, "candy, sweet food, cute, to sweeten").
wordgloss(tan, "from, by, because of, since, origin, cause").
wordgloss(taso, "only, sole, but").
wordgloss(tawa, "to, towards, in order to, for, until, to go to, to walk, to travel, to move, movement").
wordgloss(telo, "water, liquid, sauce, beverage, body fluid, body of water, to water, to wash").
wordgloss(tenpo, "time, period of time, moment, duration, situation, occasion").
wordgloss(toki, "language, speech, communication, verbal, to say, to talk, to communicate, hello!").
wordgloss(tomo, "house, room, home, building, indoor constructed space, urban, domestic").
wordgloss(tu, "two, duo, pair, to double, to separate, to cut, to divide in two").
wordgloss(unpa, "sex, sexuality, erotic, to have sex").
wordgloss(uta, "mouth, oral").
wordgloss(utala, "war, conflict, disharmony, fight, competition, attack, argument, to strike, to attack").
wordgloss(walo, "white, light colored, whiteness, lightness").
wordgloss(wan, "one, a, unit, element, particle, part, piece, to unite, to make one").
wordgloss(waso, "bird, winged animal").
wordgloss(wawa, "energy, strength, power, energetic, strong, fierce, to strengthen, to empower").
wordgloss(weka, "away, absent, missing, absence, to throw away, to remove, to get rid of").
wordgloss(wile, "to want, to need, to have to, must, will, should, desire, necessary").
