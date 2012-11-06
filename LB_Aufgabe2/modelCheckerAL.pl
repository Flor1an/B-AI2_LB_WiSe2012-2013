/***************************************************************************

              name: modelCheckerAL.pl
           version: 06.11.12
       description: A model checker for propositional logic
                    based on Chapter 1 by Patrick Blackburn & Johan Bos,
                    Vol. I and an earlier adaption by Markus Guhe and
                    Frank Schilder
            author: Carola Eschenbach (CE)
                    Christoph Klauck
          enhanced: FK
                    MS

***************************************************************************/

%:- op(850,xfx,user:(>)).         % implication
:- op(850,xfx,user:(<>)).        % biimplication 
:- op(850,yfx,user:(v)).         % disjunction
:- op(800,yfx,user:(&)).         % conjunction
:- op(750, fy,user:(~)).         % negation
%:- op(850,xfx,user:(->)).       % maps to
:- op(800,yfx,user:($)).         % NEU NAND

/* die Formel ist eine Tautologie */
tautologie(F) :-
        \+ falsifizierbar(F).  % Es gibt keine Belegung, die zu false führt

/* die Formel ist ein Widerspruch */
widerspruch(F) :-
        unerfuellbar(F).
unerfuellbar(F) :-
        \+ erfuellbar(F).  % darf nie wahr werden

/* eine Formel die weder eine Tautologie, noch ein Widerspruch ist */
kontingent(F):-
        erfuellbar(F),      % mindestens einer wahr
        falsifizierbar(F).  % mindestens einer falsch

/* es gibt Belegungen die den Wahrheitswert wahr liefern */
erfuellbar(F) :-
        aussagensymbole_L(F, []-AsL),
        asBelegung(AsL, AsB),
        evaluateAsF(F, AsB, true), !.

/* es gibt Belegungen die den Wahrheitswert falsch liefern */
falsifizierbar(F) :-
        aussagensymbole_L(F, []-AsL),
        asBelegung(AsL, AsB),
        evaluateAsF(F, AsB, false), !.
/*
        asBelegung/2: + Liste von Aussagensymbolen
                      ? Belegung
        Generiert eine Belegung, die Aussagensymbole auf Wahrheitswerte
        abbildet.
        Durch Backtracking koennen alle moeglichen Belegungen 
        generiert werden.
*/
asBelegung([], []).
asBelegung([As | Rest], [As -> true | Belegung]) :-
        asBelegung(Rest, Belegung).
asBelegung([As | Rest], [As -> false | Belegung]) :-
        asBelegung(Rest, Belegung).
 
/* evaluate AsF/3 + Formula + AsBelegung ? Value
        Berechnet den Wahrheitswert einer Aussagenlogischen Formel
        auf Basis einer Belegung der Aussagensymbole
*/

% Formeln muessen definiert sein 
evaluateAsF(Var, _, _) :- var(Var), !, fail.

% Konjuktion
evaluateAsF(Formula1 & Formula2, AsBelegung, true):-
   evaluateAsF(Formula1, AsBelegung, true),
   evaluateAsF(Formula2, AsBelegung, true), !.
   
% NEU NAND
evaluateAsF(Formula1 $ Formula2, AsBelegung, true):-
   evaluateAsF(Formula1, AsBelegung, false),
   evaluateAsF(Formula2, AsBelegung, false), !.

% Disjunktion
evaluateAsF(Formula1 v Formula2,AsBelegung, true):-
   (evaluateAsF(Formula1, AsBelegung, true);
    evaluateAsF(Formula2, AsBelegung, true)), !.

%Negation
evaluateAsF(~ Formula, AsBelegung, true):-
   evaluateAsF(Formula, AsBelegung, false), !.

% Implikation
evaluateAsF(Formula1 > Formula2, AsBelegung, true):-
   evaluateAsF((~ Formula1) v Formula2, AsBelegung, true), !.
%evaluateAsF(Formula1 > Formula2, AsBelegung, Val):-
%   evaluateAsF((~ Formula1) v Formula2, AsBelegung, Val), !.

% BiImplikation/Aequivalenz
evaluateAsF(Formula1 <> Formula2, AsBelegung, true):- 
   evaluateAsF((Formula1 > Formula2)&(Formula2 > Formula1), AsBelegung, true), !.

% Atomare Formel
evaluateAsF(Formula, AsBelegung, Value):-
   aussagensymbol(Formula), !,
   member(Formula -> Value, AsBelegung).

% Bivalenz-Prinzip der zweiwertigen Logik:
% Eine Formel ist unter einer Interpretation genau dann falsch,
% wenn sie nicht wahr ist.
evaluateAsF(F, B, false) :-
   \+ evaluateAsF(F, B, true).