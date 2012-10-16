/*************************************************************************

         name: A4.pl
      version: 16.10.2012
  description: Praktikum 1 - Aufgabe 1 - Teil 4
       author: MS
               FK

*************************************************************************/

/* ########################## Aufgabenstellung ###########################
18. Implementierung
Implementieren Sie eine einfache For-Schleife in Prolog: schleife. Sie er-
hält zwei Parameter: die Anzahl der Durchläufe und den Namen des auszu-
führenden Prädikats. Ist das Prädikat im Verlaufe der Schleifenabarbeitung
nicht ausführbar, wird eine Fehlermeldung ausgegeben. Beispiel
1 ?- schleife(5,p).
piep
piep
piep
piep
piep
true.
2 ?- schleife(5,q).
qiep
qiep
Fehler: q nicht ausführbar. (3)
true.
###########################################################################*/


schleife(Durchlauf) :-
         Durchlauf =:= 0,!.
schleife(Durchlauf) :-
         Count is (Durchlauf - 1),
         writeln('piep'),
         schleife(Count).
         
schleife :-
           write('Fehler! Bitte Durchlaufanzahl angeben'),!.

/* PROBLEM: Uns ist es nicht möglich dem Prädikat eine zweite Variable zu
übergeben im Code ( schleife(Durchlauf,Praedikat) ). Es erscheint die
Fehlermeldung "Singleton variables: [Praedikat]". Während des Praktikums
konnten wir dieses Problem nicht lösen.
Jedoch ist die Ausgabe, des ersten Beispiel der Aufgabe möglich.

PROBLEM CODE:
schleife(Durchlauf,Praedikat) :-
         Durchlauf =:= 0,!.
schleife(Durchlauf,Praedikat) :-
         Count is (Durchlauf - 1),
         writeln('piep'),
         schleife(Count,Praedikat).
 */

/******************************************************************************
*******************************************************************************

Zusätzlich bearbeitet Aufgaben (auf Papier):

Aus LBSckrit.pdf Seite 79 Nummer 11
Cut: Erklären Sie das Prinzip des Cut. Was bedeuten ein roter Cut und
ein grüner Cut bzgl. der Semantik eines Prolog-Programms ?

MOTIVATION:
Wiederholung der verschiedenen Cut Arten zur Vertiefung und sicheren Anwendung
in späteren Prolog Programmen.
PROBLEM:
Ein treffendes Beispiel für einen Grünen Cut zu finden, ohne das er als letztes
Zeichen eines Prädikats steht.


sowie **************************************************************************


Aus LBSckrit.pdf Seite 75 Nummer 2
Elemente von Prolog: Beschreiben Sie die möglichen Elemente in Prolog
(Variablen, Listen, Konstanten, Strukturen, Fakten, Regeln, Anfragen).

MOTIVATION:
Wiederholung/Erarbeitung der verschiedenen Elemente zur Vertiefung und
sicheren Anwendung in späteren Prolog Programmen.
PROBLEM:
Zeitlich bedingt konnten wir zum Ende des Praktikums noch nicht alle Elemente
recherieren/erklären.


*/