/***************************************************************************

              name: modelCheckerAL.pl
           version: 16.05.12
       description: A model checker for propositional logic
                    based on Chapter 1 by Patrick Blackburn & Johan Bos,
                    Vol. I and an earlier adaption by Markus Guhe and
                    Frank Schilder
            author: Carola Eschenbach (CE)
                    Christoph Klauck
          enhanced: FK
                    MS

***************************************************************************/

:- op(850,xfx,user:(>)).         % implication
:- op(850,xfx,user:(~>)).         % SCHWACHE implication
:- op(850,xfx,user:(<>)).        % biimplication
:- op(850,xfx,user:(~<>)).        % SCHWACHE biimplication
:- op(850,yfx,user:(v)).         % disjunction
:- op(800,yfx,user:(&)).         % conjunction
:- op(750, fy,user:(-)).         % negation
:- op(750, fy,user:(~-)).         % SCHWACHE negation


/* die Formel ist eine Tautologie */
tautologie(F) :-
        \+ falsifizierbar(F),  % Es gibt keine Belegung, die zu false führt
        \+ undefinierbar(F).   % Es gibt KEINE Belegung, die zu undef führt   Unsere Idee: bei der Tautologie MÜSSEN alle WAHR werden. Bei den anderen ist UNDEF irrelevant

fast_tautologie(F) :-
        \+ falsifizierbar(F),  % Es gibt keine Belegung die zu false führt, undef ist jedoch erlaubt.
        undefinierbar(F).

/* die Formel ist ein Widerspruch */
widerspruch(F) :-
        unerfuellbar(F);
        undefinierbar(F).
        
unerfuellbar(F) :-
        \+ erfuellbar(F).  % darf nie wahr werden

/* eine Formel die weder eine Tautologie, noch ein Widerspruch ist */
kontingent(F):-
        erfuellbar(F),       % mindestens einer wahr
        (falsifizierbar(F);  % mindestens einer falsch ODER undef   Idee: Undef wird wie False behandelt
        undefinierbar(F)).

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
        
/* es gibt Belegungen die den Wahrheitswert undef liefern */
undefinierbar(F) :-
        aussagensymbole_L(F, []-AsL),
        asBelegung(AsL, AsB),
        evaluateAsF(F, AsB, undef), !.

/*
        asBelegung/2: + Liste von Aussagensymbolen
                      ? Belegung
        Generiert eine Belegung, die Aussagensymbole auf Wahrheitswerte
        abbildet.
        Durch Backtracking koennen alle moeglichen Belegungen 
        generiert werden.
*/
asBelegung([], []).
asBelegung([As | Rest], [As -> true  | Belegung]) :-
        asBelegung(Rest, Belegung).
asBelegung([As | Rest], [As -> false | Belegung]) :-
        asBelegung(Rest, Belegung).
asBelegung([As | Rest], [As -> undef | Belegung]) :-
        asBelegung(Rest, Belegung).
 
/* evaluate AsF/3 + Formula + AsBelegung ? Value
        Berechnet den Wahrheitswert einer Aussagenlogischen Formel
        auf Basis einer Belegung der Aussagensymbole
*/

% Formeln muessen definiert sein 
evaluateAsF(Var, _, _) :- var(Var), !, fail.

% Konjuktion
evaluateAsF(Formula1 & Formula2, AsBelegung, true) :-
   evaluateAsF(Formula1, AsBelegung, true),
   evaluateAsF(Formula2, AsBelegung, true), !.

evaluateAsF(Formula1 & Formula2, AsBelegung, undef) :-
   (evaluateAsF(Formula1, AsBelegung, undef);
   evaluateAsF(Formula2, AsBelegung, undef)), !.

% Disjunktion
evaluateAsF(Formula1 v Formula2,AsBelegung, true):-
   (evaluateAsF(Formula1, AsBelegung, true);
    evaluateAsF(Formula2, AsBelegung, true)), !.

evaluateAsF(Formula1 v Formula2,AsBelegung, true):-
   (evaluateAsF(Formula1, AsBelegung, undef);
    evaluateAsF(Formula2, AsBelegung, true)), !.

evaluateAsF(Formula1 v Formula2,AsBelegung, true):-
   (evaluateAsF(Formula1, AsBelegung, true);
    evaluateAsF(Formula2, AsBelegung, undef)), !.
    
evaluateAsF(Formula1 v Formula2,AsBelegung, undef):-
   (evaluateAsF(Formula1, AsBelegung, undef);
    evaluateAsF(Formula2, AsBelegung, undef)), !.

%Negation
evaluateAsF(- Formula, AsBelegung, true):-
   evaluateAsF(Formula, AsBelegung, false), !.

evaluateAsF(- Formula, AsBelegung, false):-
   evaluateAsF(Formula, AsBelegung, true), !.

evaluateAsF(- Formula, AsBelegung, undef):-
   evaluateAsF(Formula, AsBelegung, undef), !.
   

%SCHWACHE Negation
evaluateAsF(~- Formula, AsBelegung, true):-
   (evaluateAsF(Formula, AsBelegung, false);
   evaluateAsF(Formula, AsBelegung, undef)), !.

% Implikation
evaluateAsF(Formula1 > Formula2, AsBelegung, true):-
   evaluateAsF((- Formula1) v Formula2, AsBelegung, true), !.
   
evaluateAsF(Formula1 > Formula2, AsBelegung, false):-
   evaluateAsF(-Formula1 v Formula2, AsBelegung, false), !.

evaluateAsF(Formula1 > Formula2, AsBelegung, undef):-
   evaluateAsF(-Formula1 v Formula2, AsBelegung, undef), !.

% SCHWACHE Implikation
evaluateAsF(Formula1 ~> Formula2, AsBelegung, B):-
   evaluateAsF((- Formula1) v Formula2, AsBelegung, B), !.

% BiImplikation/Aequivalenz
evaluateAsF(Formula1 <> Formula2, AsBelegung, true) :-
   evaluateAsF(Formula1 > Formula2, AsBelegung, true),
   evaluateAsF(Formula2 > Formula1, AsBelegung, true), !.

evaluateAsF(Formula1 <> Formula2, AsBelegung, false) :-
  (evaluateAsF(Formula1 > Formula2, AsBelegung, false);
  evaluateAsF(Formula2 > Formula1, AsBelegung, false)), !.
  
evaluateAsF(Formula1 <> Formula2, AsBelegung, undef) :-
  (evaluateAsF(Formula1 > Formula2, AsBelegung, undef);
   evaluateAsF(Formula2 > Formula1, AsBelegung, undef)), !.

% SCHWACHE BiImplikation/Aequivalenz
evaluateAsF(Formula1 ~<> Formula2, AsBelegung, true) :-
   evaluateAsF(Formula1 ~> Formula2, AsBelegung, true),
   evaluateAsF(Formula2 ~> Formula1, AsBelegung, true), !.
   
evaluateAsF(Formula1 ~<> Formula2, AsBelegung, true) :-
   (evaluateAsF(Formula1 ~> Formula2, AsBelegung, false);
    evaluateAsF(Formula2 ~> Formula1, AsBelegung, false)), !.
    
evaluateAsF(Formula1 ~<> Formula2, AsBelegung, true) :-
   (evaluateAsF(Formula1 ~> Formula2, AsBelegung, undef);
    evaluateAsF(Formula2 ~> Formula1, AsBelegung, undef)), !.
    
evaluateAsF(Formula1 ~<> Formula2, AsBelegung, undef) :-
   (evaluateAsF(Formula1 ~> Formula2, AsBelegung, true);
    evaluateAsF(Formula2 ~> Formula1, AsBelegung, undef)), !.

evaluateAsF(Formula1 ~<> Formula2, AsBelegung, undef) :-
   (evaluateAsF(Formula1 ~> Formula2, AsBelegung, undef);
    evaluateAsF(Formula2 ~> Formula1, AsBelegung, true)), !.
    
evaluateAsF(Formula1 ~<> Formula2, AsBelegung, true) :-
   (evaluateAsF(Formula1 ~> Formula2, AsBelegung, undef);
    evaluateAsF(Formula2 ~> Formula1, AsBelegung, false)), !.
    
evaluateAsF(Formula1 ~<> Formula2, AsBelegung, true) :-
   (evaluateAsF(Formula1 ~> Formula2, AsBelegung, false);
    evaluateAsF(Formula2 ~> Formula1, AsBelegung, undef)), !.
    


% Atomare Formel
evaluateAsF(Formula, AsBelegung, Value):-
   aussagensymbol(Formula), !,
   member(Formula -> Value, AsBelegung).

% Bivalenz-Prinzip der zweiwertigen Logik:
% Eine Formel ist unter einer Interpretation genau dann falsch,
% wenn sie nicht wahr ist.
evaluateAsF(F, B, false) :-
   \+ evaluateAsF(F, B, true),
   \+ evaluateAsF(F, B, undef).
   


/**************************************

         TRANSF
   
 **************************************/
 
 
 % Suche Beispielformel 'Number' und pruefe sie auf Wohlgeformtheit
transf(Number, ForOut) :-
  example_F(Number, Formula),
  wff(Formula),
  transf(Formula, ForOut), !.

% Bleibt gleich:
transf(F1 & F2, NF1 & NF2) :-
  transf(F1, NF1),
  transf(F2, NF2).

transf(F1 v F2, NF1 v NF2) :-
  transf(F1, NF1),
  transf(F2, NF2).

% Aendert sich:
transf(-F1, ~-NF) :-
  transf(F1, NF).

transf(F1 > F2, NF1 ~> NF2) :-
  transf(F1, NF1),
  transf(F2, NF2).

transf(F1 <> F2, NF1 ~<> NF2) :-
  transf(F1, NF1),
  transf(F2, NF2).

transf(F, NF) :-  %Atomare Formel
  NF = F.

 
 /*  TIP aus Vorlesung für teil 3

transf(F1 & F2,NF1 & NF2) :-
transf(F1,NF1),
transf(F2,NF2).

transf(~F1, -F1) :- transf(F1,NF1).

transf(F1 > F2, NF1 -> NF2) :-
transf(F1, NF1), transf(F2, NF2).
                                        */
   
   
