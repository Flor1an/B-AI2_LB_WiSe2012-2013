/*************************************************************************

         name: A2.pl
      version: 16.10.2012
  description: Praktikum 1 - Aufgabe 1 - Teil 2
       author: MS
               FK

*************************************************************************/

/*################################ Teil 2.1 ####################################
# Implementieren Sie die Prädikate and/2, or/2, nand/2, nor/2, xor/2, impl/2   #
# und aequiv/2 (für aussagenlogische Äquivalenz) als aussagenlogische          #
# Operanten zur Erweiterung des Prolog-Sprachumfangs. Diese Prädikate sollen   #
# Erfolg haben (also wahr sein) oder Scheitern (also falsch sein), wenn das    #
# Ergebnis ihrer Parameter entsprechend ihrer aussagenlogischen Semantik       #
# ausfällt. Aussagenlogische Ausdrücke können dann in Präfix-Schreibweise      #
# geschrieben werden, wie im folgenden Beispiel: and(or(A,B),nand(A,B)).       #
##############################################################################*/


% AND: wenn A und B true sind, dann true (Und)
and(A,B) :- A,B.

% OR: wenn A oder B true sind, dann true (Oder)
or(A,B) :- (A,!) ; B.

% NAND: wenn and(A,B) false, dann true (Nicht Und)
nand(A,B) :- \+( and(A,B) ).

% NOR: wenn or(A,B) false, dann true (Nicht Oder)
nor(A,B) :- \+( or(A,B) ).

% XOR: Nur wenn A oder B true sind, dann true (Entweder Oder)
xor(A,B) :- A,B ; \+(A),\+(B).

% IMPL: Aus A folgt B entspricht: Nicht-A oder B
impl(A,B) :- \+(A) ; B.

% AEQUIV: Nur wenn A und B true oder false sind, dann true
aequiv(A,B) :- and(A,B) ; \+( and(A,B) ).

/*################################ Teil 2.2 ####################################
# Implementieren Sie nun ein Prädikat tafel/3, das die Wahrheitstafel eines    #
# bestimmten logischen Ausdrucks für zwei aussagenlogische Variablen angibt.   #
# Sie können hier write, writeln, nl  und fail verwenden.                      #
##############################################################################*/

% Testaufruf:
% tafel(A,B,and(A, or(A,B))).

%%%% Wertetabelle %%%%%%
%       F1
% A B or(A,B) and(A, F1)
% 1 1   1         1
% 1 0   1         1
% 0 1   1         0
% 0 0   0         0
%%%%%%%%%%%%%%%%%%%%%%%%

tafel(true, true, C) :- write('true true | '), C -> write('true') ; write('fail').
tafel(true, false, C) :- write('true fail | '), C -> write('true') ; write('fail').
tafel(false, true, C) :- write('fail true | '), C -> write('true') ; write('fail').
tafel(false, false, C) :- write('fail fail | '), C -> write('true') ; write('fail').