/***************************************************************************

              name: modelChecker.pl
           version: 16.05.2012
       description: A model checker for predicate logic
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
:- op(800,yfx,user:($)).         % NAND
:- op(750, fy,user:(~)).         % negation

:- op(850,xfx,user:(->)).        % maps to 

/*-------------------------------------------------------------------------

satisfy(+Formula,?Example, +Variablenbelegung)
   Prueft, ob Formula in example model Example unter der Variablenbelegung
   wahr ist

--------------------------------------------------------------------------*/

satisfy(Formula, Model, VarBelegung):- 
        evaluateF(Formula, Model, VarBelegung, true).

dissatisfy(Formula, Model, VarBelegung):- 
        evaluateF(Formula, Model, VarBelegung, false).


/*
varBelegung(F, Dom, Belegung):-
        freieVariablen_F(F, Liste),
        varBelegung(Liste, Dom, Belegung).
*/
varBelegung([], _, []).
varBelegung([Var | Rest], Dom, [Var -> Value | Belegung]) :-
        var(Var),
        rndone_select(Dom,Value), % veraendert das Backtracking
        %member(Value, Dom),
        varBelegung(Rest, Dom, Belegung).
  

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
 
/* evaluateF/4 + Formula + Modell + VarBelegung ? Value
        Berechnet den Wahrheitswert einer Aussagenlogischen Formel
        auf Basis einer Belegung der Aussagensymbole

*/

% Formeln muessen definiert sein 
evaluateF(Var, _, _, _) :- var(Var), !, fail.


% Konjuktion
evaluateF(Formula1 & Formula2, Modell, VarBelegung, true):-
   evaluateF(Formula1, Modell, VarBelegung, true),
   evaluateF(Formula2, Modell, VarBelegung, true), !.
   
% NAND
evaluateF(Formula1 $ Formula2, Modell, VarBelegung, WW):-
   evaluateF(~(Formula1 & Formula2), Modell, VarBelegung, WW), !.


% Disjunktion
evaluateF(Formula1 v Formula2, Modell, VarBelegung, true):-
   (evaluateF(Formula1, Modell, VarBelegung, true);
    evaluateF(Formula2, Modell, VarBelegung, true)), !.

%Negation
evaluateF(~ Formula, Modell, VarBelegung, true):-
   evaluateF(Formula, Modell, VarBelegung, false), !.

% Implikation
evaluateF(Formula1 > Formula2, Modell, VarBelegung, WW):-
   evaluateF((~ Formula1) v Formula2, Modell, VarBelegung, WW), !.

% BiImplikation
evaluateF(Formula1 <> Formula2, Modell, VarBelegung, WW):-
   evaluateF((Formula1 > Formula2)&(Formula2 > Formula1), Modell, VarBelegung, WW), !.

% Existenzquantor
evaluateF(exists(X, Formula), Model, VarBelegung, true) :- 
   domain(Model, Domain),
   member(D, Domain), 
   evaluateF(Formula, Model, [X -> D |VarBelegung], true), !.

% Allquantor
evaluateF(forall(X, Formula), Model, VarBelegung, true) :- 
   domain(Model, Domain),
   evaluateF_all(X-Domain, Formula, Model, VarBelegung, true), !.

% Atomare Formel

% 0-stellige Relationen
evaluateF(Formula, Modell, _VarBelegung, Value):- 
   Formula =..[Pred | []], !, % 0-stellige Relation
   interpretation(Modell, Interpretation),
   member(Pred -> Value, Interpretation).

%Relationen mit mindestens einem Argument
evaluateF(Formula,Model, VarBelegung, true):- !,
   Formula =..[Pred | ArgList],
   evaluateArgs(ArgList,  Args, Model, VarBelegung),
   interpretation(Model, Interpretation),
   member(Pred -> Value, Interpretation),
   member(Args, Value).

% Bivalenz-Prinzip der zweiwertigen Logik:
% Eine Formel ist unter einer Interpretation genau dann falsch,
% wenn sie nicht wahr ist.
evaluateF(F, B, V, false) :-
   \+ evaluateF(F, B, V, true).

/* evaluateF_all(Var-Domaene, Formel, Model, VarBelegung, WW)
   prueft alle Varianten der Variablenbelegung darauf hin,
   ob sie der Formel den Wert WW zuordnen*/

evaluateF_all(_-[], _Formula, _Model, _VarBelegung, _).
evaluateF_all(X-[D | Rest], Formula, Model, VarBelegung, WW) :-
        evaluateF(Formula, Model, [X -> D |VarBelegung], WW),
        evaluateF_all(X-Rest, Formula, Model, VarBelegung, WW).

/*-------------------------------------------------------------------------

evaluateArgs(+Termliste, ?Wert ?Example, +Variablenbelegung)
   Bestimmt den Wert der Terme im Beispielmodell unter der gegebenen
   Variablenbelegung

--------------------------------------------------------------------------*/

% Nur ein Argument in der Liste
evaluateArgs([A], Value, Model, VarBelegung) :- !,
   evaluateArg(A,  Value, Model, VarBelegung).

% Werte von mehrerer Argumenter werden als Tupel ausgegeben
evaluateArgs([A | RestList],  (Value, RestArgs), Model,  VarBelegung) :- !,
   evaluateArg(A, Value, Model, VarBelegung),
   evaluateArgs(RestList, RestArgs,  Model,   VarBelegung).

/*-------------------------------------------------------------------------

evaluateArg(+Term, ?Wert ?Example, +Variablenbelegung)
   Bestimmt den Wert des Terms im Beispielmodell unter der gegebenen
   Variablenbelegung

--------------------------------------------------------------------------*/

% Variablen werden ueber die Variablenbelegung evaluiert
% == Testet, ob die Variablen gleich sind, nicht nur, ob sie unifizierbar sind
evaluateArg(A, Value,  _Model, [Var -> Value | _]) :- 
   var(A),  
   A == Var, !.

evaluateArg(A, Value, Model,  [_ | Rest]) :- 
   var(A), 
   evaluateArg(A, Value,  Model, Rest), !.


% Konstanten
evaluateArg(A, Value ,Model, _):- 
   \+ var(A),
   constant(A), !,
   interpretation(Model, Interpretation),
   member(A -> Value, Interpretation).

%Funktionssymbolen mit Argumenten
evaluateArg(A, Value,Model, VarBelegung):- !,
   \+ var(A),
   A =..[Funk | ArgList],
   evaluateArgs(ArgList,  Args, Model, VarBelegung),
   interpretation(Model, Interpretation),
   member(Funk -> FValue, Interpretation),
   member(Args -> Value, FValue).

/*-------------------------------------------------------------------------

Zufallsbelegung fuer eine freie Variable

--------------------------------------------------------------------------*/
% waehlt aus der Eingabeliste an Position K das Element aus der Liste
% catch_at(out_Element,in_Liste,in_Stelle)
catch_at(X,[X|_Xs],1). % einfachste Situation: Element an Stelle 1 genommen
catch_at(X,[_Y|Xs],K) :- K > 1,
                        K1 is K - 1,
                        catch_at(X,Xs,K1).

% waehlt per Zufall ein Element aus einer Liste
% rnd_select(in_Liste,in_Anzahl,out_Liste)
rndone_select(In,Out) :- length(In,L),NL is L + 1,
                         random(1,NL,Stelle),
                         catch_at(Out,In,Stelle).
rndone_select(In,Out) :- write('*'),rndone_select(In,Out). % unbegrenzte Reihenfolge
