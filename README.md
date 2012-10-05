# Constructed language doodles

This is a collection of Prolog programs I wrote playing with
constructed languages. Some of them are incomplete, others are just
started, a some are "done" and work as advertised.

## [blackspeech.pl](conlang/src/tip/blackspeech.pl)

This refers to Tolkien's "Black Speech", the best example is the One Ring inscription:

> Ash nazg durbatulûk, ash nazg gimbatul,
> ash nazg thrakatulûk agh burzum-ishi krimpatul. 

This is conventionally translated as:

> One Ring to rule them all, One Ring to find them,
> One Ring to bring them all and in the darkness bind them. 

I've implemented the grammatical model of this language as defined in
the Naming Language chapter of Mark Rosenfelder's book "The Language
Cosntruction Kit." He uses this as an example, but it looked fairly
simple and I wanted to know if Prolog could translate it. Turns out
the answer is yes:

    ?- [blackspeech].
    Warning: /home/fusion/Projects/Languages/Prolog/conlang/blackspeech.pl:1:
            blackspeech is not a current module (created)
    % blackspeech compiled 0.00 sec, 69 clauses
    true.
    
    blackspeech:  ?- main.
    The ring speech: ash nazg durbatulûk ash nazg gimbatul ash nazg thrakatulûk agh burzumishi krimpatul
    Parsed:
    [vp(np(noun(root(ash))), vp(np(noun(root(nazg))), vp(verb(durb, [infinitive, person(3), number(pl)])))), vp(np(noun(root(ash))), vp(np(noun(root(nazg))), vp(verb(gimb, [infinitive, person(3)])))), conj(agh, vp(np(noun(root(ash))), vp(np(noun(root(nazg))), vp(verb(thrak, [infinitive, person(3), number(pl)])))), vp(pp(ishi, np(noun(nominalized(root(burz))))), vp(verb(krimp, [infinitive, person(3)]))))].
    
    Translated:
    one ring to rule them all one ring to find them one ring to bring them all and to bind them in darkness

Let me pretty-print that parse tree so you can see what it's actually come up with there:

    [vp(
        np(
            noun(root(ash))), 
        vp(
            np(
                noun(root(nazg))), 
            vp(
                verb(durb, [infinitive, person(3), number(pl)])))), 
     vp(
        np(
            noun(root(ash))), 
        vp(
            np(
                noun(root(nazg))), 
            vp(
                verb(gimb, [infinitive, person(3)])))), 
     conj(agh, 
         vp(
             np(
                 noun(root(ash))), 
             vp(
                 np(
                     noun(root(nazg))), 
                 vp(
                      verb(thrak, [infinitive, person(3), number(pl)])))), 
         vp(
             pp(ishi, 
                 np(
                     noun(nominalized(root(burz))))), 
             vp(
                 verb(krimp, [infinitive, person(3)]))))].

The parser will actually generate five or so different parse trees,
but in practice the translation doesn't depend especially much on
which tree is selected. There's exactly one special case in the
English translation unit: `noun(nominalized(root(burz)))` is converted
directly to `darkness` rather than going through the root-lookup
facility.

## [tokipona.pl](conlang/src/tip/tokipona.pl)

This module has two parts. The first deals just with its phonology:

    ?- [tokipona].
    Warning: /home/fusion/Projects/Languages/Prolog/conlang/tokipona.pl:1:
            tokipona is not a current module (created)
    % tokipona compiled 0.01 sec, 471 clauses
    true.

    tokipona:  ?- phrase(possible_word(S), _).
    S = ma ;    S = me ;    S = mi ;    S = mo ;    S = mu ;
    S = na ;    S = ne ;    S = ni ;    S = no ;    S = nu ;
    S = pa ;    S = pe ;    S = pi ;    S = po ;    S = pu ;
    S = ta ;    S = te ;    S = ti ;    S = to ;    S = tu ;
    S = ka ;    S = ke ;    S = ki ;    S = ko ;    S = ku ;
    ...

The second part attempts to parse [Toki
Pona](http://en.wikipedia.org/wiki/Toki_Pona). I used the "ten grammar
rules" from the Wikipedia article, but there are a few errors, so as a
result I am not able to parse every sentence. But you can try parsing
some "dark teenage poetry" and see what you get:

    ?- [tokipona].
    Warning: /home/fusion/Projects/Languages/Prolog/conlang/tokipona.pl:1:
            tokipona is not a current module (created)
    % tokipona compiled 0.01 sec, 471 clauses
    true.

"Something is eating me":
    
    tokipona:  ?- tokipona([ijo, li, moku, e, mi], X).
    false.

Oops.

"I want to hurt":
    
    tokipona:  ?- tokipona([mi, wile, pakala], X).
    X = predicate([], [], mi, noun_predicate(noun(wile, [pakala]), [])) ;
    X = predicate([], [], mi, verb_predicate(modal(wile, intransitive(pakala, [])), [])) ;
    X = predicate([], [], mi, verb_predicate(intransitive(wile, [pakala]), [])) ;
    false.

You see here a good example of the inherent ambiguity of Toki Pona. Is
it saying "I [am] (want hurt)(n)"? Is it saying "I want
hurt(v. inf.)"? Is it saying "I hurt-want"? The correct answer is #2,
but the grammar is so loose there's no a priori way to determine which
translation is correct--the human must supply that information.

The problem gets worse with longer sentences: "Darkness goes inside of me":

    tokipona:  ?- tokipona([pimeja, li, tawa, insa, kon, mi], X).
    X = predicate([], [], noun_phrase(noun(pimeja, [])), noun_predicate(noun(tawa, []), [pp(insa, noun(kon, [mi]))])) ;
    X = predicate([], [], noun_phrase(noun(pimeja, [])), noun_predicate(noun(tawa, [insa, kon, mi]), [])) ;
    X = predicate([], [], noun_phrase(noun(pimeja, [])), verb_predicate(intransitive(tawa, []), [pp(insa, noun(kon, [mi]))])) ;
    X = predicate([], [], noun_phrase(noun(pimeja, [])), verb_predicate(intransitive(tawa, [insa, kon, mi]), [])) ;
    X = predicate([], [], noun_phrase(noun(pimeja, [])), verb_predicate(tawa(noun(insa, [kon, mi])), [])) ;
    false.

We got five parses this time: "Darkness is [to inside (my soul)]",
"Darkness is [inside-soul-me to]", "Darkness goes [into [my soul]]",
"Darkness [inside my soul]-ly goes", "Darkness goes-to [my soul]
inside". In practice several of these are close enough that a fairly
direct English translation would be fine, but a few weirdly miss the
mark, mostly involving the zero copula.

Let's try "Nobody can know my pain":

    tokipona:  ?- tokipona([jan, ala, li, ken, sona, e, pilin, ike, mi], X).
    X = predicate([], [], noun_phrase(noun(jan, [ala])), verb_predicate(modal(ken, transitive(sona, [], noun(pilin, [ike, mi]))), [])) ;
    X = predicate([], [], noun_phrase(noun(jan, [ala])), verb_predicate(transitive(ken, [sona], noun(pilin, [ike, mi])), [])) ;
    false.

Two parses: "Nobody can know my-bad-feeling" and "Nobody knowingly-can
my-bad-feeling." Pretty close. One more: "Poetry, you are my one and
only friend":
    
    tokipona:  ?- tokipona([toki, musi, o, sina, jan, pona, mi, wan, taso], X).
    X = vocative([], noun(toki, [musi]), verb_predicate(intransitive(sina, [jan, pona, mi, wan, taso]), [])) ;
    X = predicate([], [noun(toki, [musi])], sina, noun_predicate(noun(jan, [pona, mi, wan, taso]), [])) ;
    X = predicate([], [noun(toki, [musi])], sina, verb_predicate(intransitive(jan, [pona, mi, wan, taso]), [])) ;
    false.

"O Poetry! Only-one-my-friend you" looks pretty bad. The other two are
better: "O Poetry, you are only-one-my-friend" and "O Poetry, you
only-one-friendly person(v)". I think all three of these actually
capture the meaning pretty well, but only the second one will yield a
meaningful English translation.

The rest of the poem doesn't parse quite yet.

## [phono.pl](conlang/src/tip/phono.pl)

This is an attempt to handle phonology in a rather general fashion. Very incompelete.

## [syllab.pl](conlang/src/tip/syllab.pl)

This is an attempt to handle syllables and phonotactics in a general fashion. Also very incomplete.
