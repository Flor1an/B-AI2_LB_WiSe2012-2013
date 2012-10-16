/*************************************************************************

         name: A3.pl
      version: 16.10.2012
  description: Praktikum 1 - Aufgabe 1 - Teil 3
       author: MS
               FK

*************************************************************************/

/*################################# Teil 3 #####################################
# Lösen von Logikproblemen: Wählen Sie sich irgendeine Logikaufgabe aus        #
# (auch im WWW), die Sie versuchen, mit Prolog zu lösen. Achtung: Setzen Sie   #
# sich ein Zeitlimit und dokumentieren Sie ggf. warum es nicht oder nur        #
# schlecht oder gut mit Hilfe von Prolog zu lösen war.                         #
############################################################################# */


% Aufgabe aus dem WWW (http://www.techfreaq.de/informatikLogik.htm)
% Lösung auf der Seite ist Teils fehlerhaft.


% Wenn, Dann (Wenn es regnet(A), dann ist die Starße nass(B)) 

% A B  A->B
% 1 1   1
% 1 0   0
% 0 1   1
% 0 0   1

% A -> B = -A v B    (Vereinfachung der Implikation)

% Aus Teil 2
impl(A,B) :- \+(A) ; B.

% Aufrufmöglichekeiten:
% impl(true,true): Es regnet und die Straße ist nass.
% impl(true,false): Es regnet und die Straße ist nicht nass.
% impl(false,true): Es regnet nicht und die Straße ist nass.
% impl(false,false): Es regnet nicht und die Straße ist nicht nass.