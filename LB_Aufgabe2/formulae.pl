/*************************************************************************

         name: formulae.pl 
      version: 06.11.12
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

%:- op(850,xfx,user:(>)).         % implication
:- op(850,xfx,user:(<>)).        % biimplication 
:- op(850,yfx,user:(v)).         % disjunction
:- op(800,yfx,user:(&)).         % conjunction
:- op(750, fy,user:(~)).         % negation
:- op(800,yfx,user:($)).         % NEU NAND

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
   
% NEU NAND
wff(Formula1 $ Formula2):- !,
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

% Bi-Implikation/Aequivalenz
wff(Formula1 <> Formula2):- !,
   wff(Formula1),
   wff(Formula2).

% Negation
wff(~ Formula):- !,
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
aussagensymbole_L(~ For, LIO) :-
        aussagensymbole_L(For, LIO).
aussagensymbole_L(For1 & For2, LIn-LOut) :-
        aussagensymbole_L(For1, LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 $ For2, LIn-LOut) :-         %NEU NAND
        aussagensymbole_L(For1, LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 v For2, LIn-LOut) :-
        aussagensymbole_L(For1, LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 > For2, LIn-LOut) :-
        aussagensymbole_L(For1,  LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).
aussagensymbole_L(For1 <> For2, LIn-LOut) :-
        aussagensymbole_L(For1,  LIn-VarMid),
        aussagensymbole_L(For2, VarMid-LOut).

/* atomare Formeln */
aussagensymbole_L(Formula,  LIn-LIn):-
  aussagensymbol(Formula),
  member(Formula, LIn), !.

aussagensymbole_L(Formula,  LIn-[Formula |LIn]):-
  aussagensymbol(Formula),
  \+ member(Formula, LIn), !.
  
  
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


coco(Formel1 & Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),                      % Teilformel1 auswerten, Output: Counter, ListeOJunkt
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),                      % Teilformel2 auswerten, Output: Counter, ListeOJunkt
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,                    % Counter addieren
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunktNeu1),              % Listen zusammmenführen
          append(ListeOJunktNeu1,[Formel1 & Formel2],ListeOJunkt).        % Teilformeln in Liste speichern


coco(Formel1 v Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).%Neu1),
          %append(ListeOJunktNeu1,[Formel1 v Formel2],ListeOJunkt).

coco(Formel1 $ Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).%Neu1),
          %append(ListeOJunktNeu1,[Formel1 v Formel2],ListeOJunkt). % Teilformeln in Liste speichern

coco(Formel1 <> Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).%Neu1),
          %append(ListeOJunktNeu1,[Formel1 v Formel2],ListeOJunkt). % Teilformeln in Liste speichern

coco(Formel1 -> Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).%Neu1),
          %append(ListeOJunktNeu1,[Formel1 v Formel2],ListeOJunkt). % Teilformeln in Liste speichern


coco(~Formel1, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu, ListeOJunkt),
          AnzJunkt is AnzJunktNeu + 1.  % Teilformel1 auswerten, Output: Counter, ListeOJunkt

coco(_Praedikat,0,[]).


