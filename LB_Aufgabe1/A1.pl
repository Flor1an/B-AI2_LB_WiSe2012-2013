/*************************************************************************

         name: A1.pl
      version: 16.10.2012
  description: Praktikum 1 - Aufgabe 1 - Teil 1
       author: MS
               FK

*************************************************************************/

/*################################# Teil 1 #####################################
# Lösen von Aufgaben der Aussagenlogik: Bei den Folien zur Aussagenlogik in    #
# der Datei KLogik.pdf wird am Beispiel einer Modellierungsaufgabe (eine Firma #
# und ihre Teams) aufgezeigt, wie eine solche Aufgabe mit Prolog gelöst werden #
# kann. Wählen Sie aus dem Skript eine Aufgabe (das Resolutions-Kalkül         #
# betreffend) aus, lösen Sie diese mittels Prolog.                             #
############################################################################# */

% Beispielaufgabe von der Folie 25,26 (KLogik.pdf)
% Frage: ?- f.

% {{a,¬c}1, {b,¬c}2, {d,¬a,¬e}3, {f,¬b,¬d}4, {c}5, {e}6, {¬f}7}
% Aufruf über: trace. f.
% K4 u K7 --> {¬b,¬d}R1 - f fliegt raus
% R1 u K2 --> {¬d}R2 - b fliegt raus
% ...

a :- c.       %entspricht C --> A
b :- c.       %entspricht C --> B
d :- a, e.    %entspricht (A & E) --> D
f :- b, d.    %entspricht (B & D) --> F
c.            %
e.            %entspricht (C & E) --> F   >>>> F ist die Anfrage (F wenn C und E)