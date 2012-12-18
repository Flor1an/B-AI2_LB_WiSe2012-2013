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

%:- op(850,xfx,user:(>)).         % implication 
:- op(850,xfx,user:(<>)).        % biimplication 
:- op(850,yfx,user:(v)).         % disjunction
:- op(800,yfx,user:(&)).         % conjunction
:- op(800,yfx,user:($)).         % NAND
:- op(750, fy,user:(~)).         % negation


/*-------------------------------------------------------------------------
Wohlgeformte Formeln

wff (+Formula)

--------------------------------------------------------------------------*/

% Formeln muessen definiert sein
% Formeln sind keine Funktionsvariablen!
wff(Var) :- var(Var), !, fail.

% Konjuktion
wff(Formula1 & Formula2):- !,
   wff(Formula1),
   wff(Formula2).
   
% NAND
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

% Bi-Implikation
wff(Formula1 <> Formula2):- !,
   wff(Formula1),
   wff(Formula2).

%Negation
wff(~ Formula):- !,
   wff(Formula).

% Praedikatenlogik

% Existenzquantor
wff(exists(X, Formula)) :- !, 
   wff(Formula), var(X).

% Allquantor
wff(forall(X, Formula)) :- !,
   wff(Formula), var(X).

% Atomare Formel
% Relationssymbole und richtige Anzahl von Termen
wff(Formula):-
   Formula =..[Pred | ArgList],
   relation(Pred, Arity),
   length(ArgList, Arity), !,
   wfts(ArgList).

% Fehlerbehandlung
wff(Formula) :- write('WFF-Fehler! Keine gueltige Formel:'),
   writeln(Formula),!,fail.

/*-------------------------------------------------------------------------
Listen von Wohlgeformten Termen

wfts(+Termliste)
   Termliste (nicht leer)
--------------------------------------------------------------------------*/
% Kein Argument in der Liste
wfts([]). % ermoeglicht 0-stellige Relationen

% Mindestens ein Argument
wfts([A | RestList]) :- !,
   wft(A),
   wfts(RestList).

/*-------------------------------------------------------------------------
Wohlgeformte Terme

wft(+Term)
  Terme, hier Konstanten und Variablen
--------------------------------------------------------------------------*/

% Funktionsvariablen sind Prolog-Variablen
wft(A) :- 
   var(A),  !.

% Konstanten  
wft(A) :- 
   \+var(A), 
   constant(A), !.
 
% komplexer Term: Funktionsssymbole und richtige Anzahl von Termen
wft(T):- !,
   \+var(T), 
   T =..[Funk | ArgList],
   function(Funk, Arity),
   length(ArgList, Arity),
   wfts(ArgList).
   
% Fehlerbehandlung
wft(Formula) :- write('WFF-Fehler! Keine gueltiger Term:'),
   writeln(Formula),!,fail.


/*-------------------------------------------------------------------------

freieVariablen_F(+Formel, ?Variablenliste)
   generiert die Liste der in der Formel vorkommenden freien
   Variablen

   Da eine Variable in einer Formel mehrfach frei vorkommen kann, in der 
   Liste aber nur einmal gefuehrt werden soll, wird mit In-Out-Listen 
   gearbeitet.
--------------------------------------------------------------------------*/

freieVariablen_F(For,_) :- var(For), !, fail.
freieVariablen_F(~ For, VIO) :-
        freieVariablen_F(For, VIO).
freieVariablen_F(For1 & For2, VarIn-VarOut) :-
        freieVariablen_F(For1, VarIn-VarMid),
        freieVariablen_F(For2, VarMid-VarOut).
freieVariablen_F(For1 $ For2, VarIn-VarOut) :-
        freieVariablen_F(For1, VarIn-VarMid),
        freieVariablen_F(For2, VarMid-VarOut).
freieVariablen_F(For1 v For2, VarIn-VarOut) :-
        freieVariablen_F(For1, VarIn-VarMid),
        freieVariablen_F(For2, VarMid-VarOut).
freieVariablen_F(For1 > For2, VarIn-VarOut) :-
        freieVariablen_F(For1,  VarIn-VarMid),
        freieVariablen_F(For2, VarMid-VarOut).
freieVariablen_F(For1 <> For2, VarIn-VarOut) :-
        freieVariablen_F(For1,  VarIn-VarMid),
        freieVariablen_F(For2, VarMid-VarOut).

/* Quantoren binden Variablen. Daher wird hier die Variable fuer
   die eingebettete Formel in die In-Liste geschrieben, am Ende aber 
   nicht nach aussen weiter gegeben 
   Quantoren behandeln die Liste also wie ein Stack, freie 
   Variablen werden dann hinten angehaegt, sie behandeln die Liste wie eine
   Queue*/

freieVariablen_F(exists(X, Formula), VarIn-VarOut) :- 
        freieVariablen_F(Formula, 
                                 [X | VarIn]-[X |VarOut]).
freieVariablen_F(forall(X, Formula), VarIn-VarOut) :- 
        freieVariablen_F(Formula, 
                                 [X | VarIn]-[X |VarOut]).
/* atomare Formeln */
freieVariablen_F(Formula,  VIO):-
  Formula =..[Pred | ArgList],
  \+ member(Pred, [& , v , > , ~ , exists, forall]),
  freieVariablen_TL(ArgList, VIO).


/*-------------------------------------------------------------------------

freieVariablen_TL(+Termliste, +Domäne ?Variablenliste)
   generiert die Liste der in der Termliste vorkommenden 
   freien Variablen

--------------------------------------------------------------------------*/

freieVariablen_TL([], V-V) :- !.

freieVariablen_TL([A | RestList], VarIn-VarOut) :- 
  freieVariablen_T(A, VarIn-VarMid ),
  freieVariablen_TL(RestList, VarMid-VarOut).

/*-------------------------------------------------------------------------

freieVariablen_T(+Term, +Domäne ?Variablenliste)
   generiert en fuer die in dem Term vorkommenden 
   freien Variablen. Derzeit kann das nur der Term selbst sein

--------------------------------------------------------------------------*/

/* Eine Variable, die schon in der Liste steht. Sie ist also entweder 
   gebunden oder (frei und schon mal aufgetaucht) */

freieVariablen_T(A,   [Var | Rest]-[Var | Rest]) :- 
        var(A),  
        A == Var, !.
 
freieVariablen_T(A,  [VV | RestIn]-[VV | RestOut]) :- 
        var(A), 
        freieVariablen_T(A, RestIn-RestOut).

/* eine freie Variable, die noch nicht in der Liste stand. 
   Sie wird ans Ende angefuegt */

freieVariablen_T(A, []-[A]) :- 
        var(A), !.


/* konstante*/

freieVariablen_T(A,  V-V) :- 
        \+var(A), constant(A),  ! .


freieVariablen_T(A,  VIO):-
        \+var(A), 
        A =..[_ | ArgList],
        freieVariablen_TL(ArgList, VIO).
        

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %Aufgabe COCO  | Beispielaufruf: coco(forall(X, a&b v (c->(d&b))) ,F,G).%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
coco(exists(_, Formel1), AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunkt, ListeOJunkt).

coco(forall(_, Formel1), AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunkt, ListeOJunkt).
          
          
coco(Formel1 & Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),                      % Teilformel1 auswerten, Output: Counter, ListeOJunkt
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),                      % Teilformel2 auswerten, Output: Counter, ListeOJunkt
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,                    % Counter addieren
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunktNeu1),              % Listen zusammmenführen
          append(ListeOJunktNeu1,[Formel1 & Formel2],ListeOJunkt).        % Teilformeln in Liste speichern
          
coco(Formel1 $ Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).
          
coco(Formel1 v Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).
          
coco(Formel1 <> Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).
          
coco(Formel1 -> Formel2, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu1, ListeOJunkt1),
          coco(Formel2, AnzJunktNeu2, ListeOJunkt2),
          AnzJunkt is AnzJunktNeu1 + AnzJunktNeu2 + 1,
          append(ListeOJunkt1,ListeOJunkt2,ListeOJunkt).
          


coco(~Formel1, AnzJunkt, ListeOJunkt) :- !,
          coco(Formel1, AnzJunktNeu, ListeOJunkt),
          AnzJunkt is AnzJunktNeu + 1.
          
coco(_Praedikat,0,[]).






