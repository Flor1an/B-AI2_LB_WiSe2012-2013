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
h�lt zwei Parameter: die Anzahl der Durchl�ufe und den Namen des auszu-
f�hrenden Pr�dikats. Ist das Pr�dikat im Verlaufe der Schleifenabarbeitung
nicht ausf�hrbar, wird eine Fehlermeldung ausgegeben. Beispiel
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
Fehler: q nicht ausf�hrbar. (3)
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

/* PROBLEM: Uns ist es nicht m�glich dem Pr�dikat eine zweite Variable zu
�bergeben im Code ( schleife(Durchlauf,Praedikat) ). Es erscheint die
Fehlermeldung "Singleton variables: [Praedikat]". W�hrend des Praktikums
konnten wir dieses Problem nicht l�sen.
Jedoch ist die Ausgabe, des ersten Beispiel der Aufgabe m�glich.

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

Zus�tzlich bearbeitet Aufgaben (auf Papier):

Aus LBSckrit.pdf Seite 79 Nummer 11
Cut: Erkl�ren Sie das Prinzip des Cut. Was bedeuten ein roter Cut und
ein gr�ner Cut bzgl. der Semantik eines Prolog-Programms ?

MOTIVATION:
Wiederholung der verschiedenen Cut Arten zur Vertiefung und sicheren Anwendung
in sp�teren Prolog Programmen.
PROBLEM:
Ein treffendes Beispiel f�r einen Gr�nen Cut zu finden, ohne das er als letztes
Zeichen eines Pr�dikats steht.


sowie **************************************************************************


Aus LBSckrit.pdf Seite 75 Nummer 2
Elemente von Prolog: Beschreiben Sie die m�glichen Elemente in Prolog
(Variablen, Listen, Konstanten, Strukturen, Fakten, Regeln, Anfragen).

MOTIVATION:
Wiederholung/Erarbeitung der verschiedenen Elemente zur Vertiefung und
sicheren Anwendung in sp�teren Prolog Programmen.
PROBLEM:
Zeitlich bedingt konnten wir zum Ende des Praktikums noch nicht alle Elemente
recherieren/erkl�ren.


*/