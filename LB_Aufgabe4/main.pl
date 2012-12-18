/*************************************************************************

         name: main.pl 
      version: 16.05.2012
  description: Testpraedikate fuer die Praedikatenlogik
       author: Carola Eschenbach (CE)
               Christoph Klauck
     enhanced: FK
               MS
 
*************************************************************************/
:- module(main,[testAll/0, test/2, test_F/2]).

%:- use_module(comPredicates). % substitute/4

:- ensure_loaded(exampleModels /*,[constant/1,relation/2, function/2,
                             domain/2, interpretation/2, 
                             example_F/2]*/).
:- ensure_loaded(formulae).

:- ensure_loaded(modelChecker).

%:- ensure_loaded(loesungPL).

/*-------------------------------------------------------------------------
     Semantic Evaluation
    -----------------------


 Diese Variante kann auch mit freien Variablen in der Formel umgehen.

testAll/0
 
test(+Example,?Model)
   Evaluates Formula in example model Example 

--------------------------------------------------------------------------*/
% ACHTUNG: hierzu vorher die Zeile
% rndone_select(In,Out) :- write('*'),rndone_select(In,Out). % unbegrenzte Reihenfolge
% am Ende in modelChecker.pl loeschen / auskommentieren
% sonst gibt es eine Endlosschleife
testAll:-
   example_F(_Example,Formula), % Waehle das Beispiel
   domain(Model, _Domain),
   test_F(Formula, Model), fail.

testAll.

test(Example, Model):-
   example_F(Example,Formula), % Waehle das Beispiel
   test_F(Formula, Model) .

/*------------------------------------------------------------------------- 
test_F(+Formula,?Model)
   Evaluates Formula in example model Example 

--------------------------------------------------------------------------*/

test_F(Formula, Model):-
   wff(Formula), % evaluiere nur wohlgeformte Formeln
   domain(Model, Domain),    % hole die Domaene
   interpretation(Model, Interpretation), % hole die Interpretation
   freieVariablen_F(Formula, []-VarListe),
   varBelegung(VarListe, Domain, VarBelegung),
                             % generiere eine Variablenbelegung fuer die
                             % freien Variablen
   copy_term(Formula-VarBelegung, FormulaL-VarBelegungL),
   numbervars(FormulaL-VarBelegungL,0,_),  
   write('Die Formel '),  writeln(FormulaL), 
   write('ist in der Domaene '),  writeln(Domain), 
   write('unter der Interpretation '), writeln(Interpretation), 
   write('und der Variablenbelegung '), writeln(VarBelegungL), 
   ((satisfy(Formula,Model, VarBelegung), % Puefe, ob die Formel erfuellt ist
     writeln('erfuellt.')); % und gib das Ergebnis bekannt
   (dissatisfy(Formula,Model, VarBelegung), % Puefe, ob die Formel nicht erfuellt ist
    writeln('nicht erfuellt.'))) . % und gib das Ergebnis bekannt

test_F(Formula, _):-
   \+ wff(Formula), % evaluiere nur wohlgeformte Formeln
   write('Der Ausdruck '),  writeln(Formula), 
   write('ist fuer diese Version des Modell-Pruefers nicht evaluierbar ') .
