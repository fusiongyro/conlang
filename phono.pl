:- module(phono).

manner(stop).
manner(nasal).
manner(fricative).
manner(sibilant).
manner(lateral).
manner(affricate).
manner(flap).
manner(trill).
manner(approximant).

passive_place(labial).
passive_place(dental).
passive_place("dental-alveolar").
passive_place(alveolar).
passive_place("post-alveolar").
passive_place(palatal).
passive_place(velar).
passive_place(uvular).
passive_place(pharyngeal).
passive_place(epiglottal).

active_place(labial).
active_place(apical).
active_place(laminal).
active_place(subapical).
active_place(dorsal).
active_place(radical).
active_place(epiglottal).
active_place(larynx).
active_place(glottis).

place(retroflex).
place(X) :- passive_place(X).
place(X) :- active_place(X).

phonation(voiceless).
phonation(voiced).
phonation(creaky).

nasality(nasal).
nasality(oral).

exit(central).
exit(lateral).
exit(na).

airstream(pulmonic).
airstream("glottalic egressive").
airstream("glottal ingressive").
airstream("lingual ingressive").

% consonant(IPA, ArticulationManner, ArticulationPlace, Phonation, Nasality, Exit, AirstreamMechanism)

consonant(ʈ, stop, retroflex, voiceless, oral, na, pulmonic).
consonant(z, sibilant, alveolar, voiced, oral, central, pulmonic).
