/*************************************************************************

         name: main.pl 
      version: 06.11.2012
  description: Testpraedikate fuer die Aussagenlogik 
       author: Carola Eschenbach (CE)
               Christoph Klauck
     enhanced: FK
               MS
 
*************************************************************************/
:- module(main,[test_tt/1, test/1]).

:- ensure_loaded(examples). %aussagensymbol/1, example_F/2.

:- ensure_loaded(formulae). %wff/1, aussagensymbole_L/2

:- ensure_loaded(modelCheckerAL). %tautologie/1, widerspruch/1, kontingent/1,
                                  %asBelegung/2, evaluateAsF/3

%:- ensure_loaded(loesungAL). %aussagensymbol/1, example_F/2.

/*-------------------------------------------------------------------------
     Semantic Evaluation
    -----------------------

     nur Aussagenlogik

     prueft, ob eine Formel eine Tautologie, Widerspruch oder
     erfüllbar ist und gibt das Ergebnis aus.

--------------------------------------------------------------------------*/


test(N):-
%    var(N),
    example_F(N, Formula),
    test(Formula).

test(F) :-
        wff(F)
        -> ( write(F),
           (tautologie(F) -> write(' ist eine Tautologie. ');
            widerspruch(F) -> write(' ist ein Widerspruch. ');
            kontingent(F) -> write(' ist kontingent. ')))
        ; ( write('Der Ausdruck '),  writeln(F), 
         write('ist fuer diese Version des Modell-Pruefers nicht evaluierbar ')).

/*-------------------------------------------------------------------------
test_tt(True v True)

    gibt den Wahrheitswertverlauf einer Formel aus.

--------------------------------------------------------------------------*/

test_tt(N):-
%    var(N),
    example_F(N, Formula),
    test_tt(Formula).

test_tt(Formula):-
   wff(Formula)  % evaluiere nur wohlgeformte Formeln
     -> (aussagensymbole_L(Formula, []-AsListeUS),sort(AsListeUS,AsListe),
         nl,
         ttHeader(Formula, AsListe),
         ttBorder(Formula, AsListe),
         (   asBelegung(AsListe, AsBelegung),
                             % generiere eine Belegung fuer die
                             % aussagensymbole
             evaluateAsF(Formula, AsBelegung, Value),
             ttLine(Value, AsBelegung),
             fail) 
          ; true)
       ; ( write('Der Ausdruck '),  writeln(Formula), 
         write('ist fuer diese Version des Modell-Pruefers nicht evaluierbar ')).


/*------------------------------------------------------------------------- 
 
    Hilfspraedikate fuer die Ausgabe des Wahrheitswertverlaufs einer Formel

--------------------------------------------------------------------------*/
% Kopfzeile

ttHeader(F, []) :- write('| '), writeln(F).
ttHeader(F, [A | L]) :- write(A), write(' '), ttHeader(F, L).
% horizontale Linie
ttBorder(F, []) :- term_to_atom(F, A), atom_length(A, N), 
        write('+-'), wline(N), nl.
ttBorder(F, [A | L]) :- atom_length(A, N), 
         wline(N), wline(1), ttBorder(F, L).
wline(0).
wline(1) :- !, write('-').
wline(N) :- !, N > 1, N1 is N-1, write('-'), wline(N1).

% Zeile der Wahrheitstafel
ttLine(true, []) :- write('| '), writeln(t).
ttLine(false, []) :- write('| '), writeln(f).
ttLine(V, [A -> true | L]) :- 
        atom_length(A, N), 
        write(t), tab(N), 
        ttLine(V, L).
ttLine(V, [A -> false | L]) :- 
        atom_length(A, N), 
        write(f), tab(N), 
        ttLine(V, L).

% Fuer dreiwertige Logik:
ttLine(indef, []) :- write('| '), writeln(i).
ttLine(V, [A -> indef | L]) :- 
        atom_length(A, N), 
        write(i), tab(N), 
        ttLine(V, L).
