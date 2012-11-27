/*************************************************************************

         name: formulae.pl 
      version: 16.05.12
  description: Uses a number of ideas from Melvin Fitting's
               implementation of an unsigned tableaux theorem prover 
               for first-order logic, in "First-Order Logic and 
               Automated Theorem Proving", Second Edition (1996),
               Graduate Texts in Computer Science, Springer.
               For more details, see the Notes to Chapter 5.
      authors: Carola Eschenbach (CE)
               Christoph Klauck
     enhanced: FK
               MS

*************************************************************************/
 
% Junktoren als Prolog-Operatoren

:- op(850,xfx,user:(>)).         % implication
:- op(850,xfx,user:(~>)).         % SCHWACHE implication
:- op(850,xfx,user:(<>)).        % biimplication
:- op(850,xfx,user:(~<>)).        % SCHWACHE biimplication
:- op(850,yfx,user:(v)).         % disjunction
:- op(800,yfx,user:(&)).         % conjunction
:- op(750, fy,user:(-)).         % negation
:- op(750, fy,user:(~-)).         % SCHWACHE negation

/*-------------------------------------------------------------------------
Wohlgeformte Formeln

wff (+Formula)
--------------------------------------------------------------------------*/

% Formeln muessen definiert sein
% Prolog-Variablen sind keine Formeln!
wff(Var) :- var(Var), !, fail.

% Konjuktion
wff(Formula1 & Formula2):- !,
   wff(Formula1),
   wff(Formula2).

% Disjunktion
wff(Formula1 v Formula2):- !,
   wff(Formula1),
   wff(Formula2).

% Implikation
wff(Formula1 > Formula2):- !,
   wff(Formula1),
   wff(Formula2).

% SCHWACHE Implikation
wff(Formula1 ~> Formula2):- !,
   wff(Formula1),
   wff(Formula2).

% Bi-Implikation/Aequivalenz
wff(Formula1 <> Formula2):- !,
   wff(Formula1),
   wff(Formula2).

% SCHWACHE Bi-Implikation/Aequivalenz
wff(Formula1 ~<> Formula2):- !,
   wff(Formula1),
   wff(Formula2).

% Negation
wff(- Formula):- !,
   wff(Formula).
   
% SCHWACHE Negation
wff(~- Formula):- !,
   wff(Formula).

% Atomare Formel
wff(Formula):-
   aussagensymbol(Formula), !.

% Fehlerbehandlung
wff(Formula) :- write('WFF-Fehler! Keine gueltige Formel:'),
   writeln(Formula),!,fail.

/*-------------------------------------------------------------------------

aussagensymbole_L(+Formel, +ListeIn-?ListeOut)
   generiert die Liste der in der Formel vorkommenden freien
   Variablen

   Da ein Aussagensymbol in einer Formel mehrfach vorkommen kann,
   in der Liste aber nur einmal gefuehrt werden soll, wird mit
   In-Out-Listen gearbeitet.
--------------------------------------------------------------------------*/

aussagensymbole_L(For,_) :- var(For), !, fail.
aussagensymbole_L(- For, LIO) :-
        aussagensymbole_L(For, LIO).
aussagensymbole_L(~- For, LIO) :- % SCHWACHE Negation
        aussagensymbole_L(For, LIO).
aussagensymbole_L(For1 & For2, LIn-LOut) :-
        aussagensymbole_L(For1, LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 v For2, LIn-LOut) :-
        aussagensymbole_L(For1, LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 > For2, LIn-LOut) :-
        aussagensymbole_L(For1,  LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 ~> For2, LIn-LOut) :- % SCHWACHE Implikation
        aussagensymbole_L(For1,  LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 <> For2, LIn-LOut) :-
        aussagensymbole_L(For1,  LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 ~<> For2, LIn-LOut) :-  % SCHWACHE Bi-Implikation/Aequivalenz
        aussagensymbole_L(For1,  LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).

/* atomare Formeln */
aussagensymbole_L(Formula,  LIn-LIn):-
  aussagensymbol(Formula),
  member(Formula, LIn), !.

aussagensymbole_L(Formula,  LIn-[Formula |LIn]):-
  aussagensymbol(Formula),
  \+ member(Formula, LIn), !.
  
  
