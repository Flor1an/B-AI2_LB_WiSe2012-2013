/******************************************************************************

              name: exampleModels.pl
           version: 16.05.2012
       description: Some example models defined over a vocabulary
            author: based on modelFOL.pl (Chapter 1) by 
                    Patrick Blackburn & Johan Bos Vol. I
                    Christoph Klauck
          enhanced: FK
                    MS

******************************************************************************/

:- module(exampleModels,[constant/1,relation/2, function/2,
                         interpretation/2, domain/2, 
                         example_F/2]).

/* ==================================================================
 Signatur einer logischen Sprache
===================================================================*/
/* 
Operators : Diese Symbole werden auch in anderen Programmteilen verwendet
und die Operatorenspezifikation vorausgesetzt.
Diese Deklaration sollte in allen beispiedateien gleich sein.

*/
%:- op(850,xfx,user:(>)).         % implication 
:- op(850,xfx,user:(<>)).        % biimplication 
:- op(850,yfx,user:(v)).         % disjunction
:- op(800,yfx,user:(&)).         % conjunction
:- op(800,yfx,user:($)).         % NAND
:- op(750, fy,user:(~)).         % negation

%:- op(850,xfx,user:(->)).        % maps to


/* %%%%%%%%%%%%%%%% TEILAUFGABE 4 %%%%%%%%%%%%%%%%%%%%%%%

7. Struktur (von Seite 108)
7. Struktur (? Punkte)
Welche der folgenden Strukturen sind Modelle für die Formel F ?
F = ExEyEz : [P(x, y) & P(z, y) & P(x, z) & ¬P(z, x)]

(a) A = (UA, IA), wobei UA = N, PA = {(m, n)|m, n € N & m < n}.
*/


example_F(a1(1), exists(X, exists(Y, exists(Z, p(X,Y) & p(Z,Y) & p(X,Z) & ~( p(Z,X)))))).


domain(i1, [1,2,3]).  %Universum
                      %x=1   y=2   z=3
interpretation(i1, [p -> [(1,2), (2,3), (1,3)]]).



% Aufruf: test(a1(1), i1).


/*-----------------------------------------------------------------------------

Vocabulary

-----------------------------------------------------------------------------*/


% constant ?Atom
% koennten auch als 0-stellige Funktionen umgesetzt werden
constant(peter).
constant(laura).

% function ?Atom ?Stelligkeit
function(mutter, 1).
function(f, 1).

% relation ?Atom ?Stelligkeit
relation(es_regnet,0).
relation(a,0).
relation(b,0).
relation(c,0).

relation(junge,1).
relation(haus,1).         
relation(winkt,1).         
relation(lacht,1).         
relation(rot,1).             

relation(sieht,2).
relation(p,2).
relation(p1,3).
relation(q,1).
relation(r,2).
relation(s,1).

% Beispiel-Formeln
example_F(e1, lacht(peter)).
example_F(e2, sieht(peter, laura) v lacht(laura)).
example_F(e3, sieht(peter, A) v lacht(A)).
example_F(e3b, forall(A, sieht(peter, A) v lacht(A))).
example_F(e4, forall(X, junge(X) > lacht(X))).
example_F(e5, exists(X, junge(X) & winkt(X))).
example_F(e6, ~ forall(X, junge(X) > lacht(X))).
example_F(e7, ~ exists(X, junge(X) & winkt(X))).
example_F(e8, ~ exists(X, junge(X) & ~ lacht(X))).
example_F(e9, forall(X, sieht(X, A) v sieht(A, X))).
example_F(e10, forall(X, (~ es_regnet) > lacht(X))).
example_F(e11, forall(X, (~ lacht(X)) > (~ lacht(mutter(X))))).

%example_F(f(1), forall(X, exists(Y,p1(X,Y,f(_Z))))). %i4, i5
example_F(f(2), forall(X, p(X,X))). %i6, i7, i8
example_F(f(3), forall(X,forall(Y,p(X,Y) > p(Y,X)))). % i7, i6, i8
example_F(f(4), forall(X,forall(Y,forall(Z,(p(X,Y) & p(Y,Z)) > p(X,Z))))). %i8, i6, i7
example_F(f(5), (forall(X,exists(Y,r(X,Y))) & forall(X,exists(Y,~ r(X,Y))))
                & forall(X,forall(Y,forall(Z,(r(X,Y) & r(Y,Z)) > r(X,Z))))). %i9
example_F(f(7), forall(X,exists(Y,r(X,Y)))).   %i9
example_F(f(8), forall(X,exists(Y,~ r(X,Y)))). %i9
example_F(f(9), forall(X,forall(Y,forall(Z,(r(X,Y) & r(Y,Z)) > r(X,Z))))). %i9
example_F(f(10),((exists(X,s(X)) & exists(Y,q(Y))) > (exists(Z,s(Z) & q(Z))))). % i10,i11


/*-----------------------------------------------------------------------------

 Example Models

-----------------------------------------------------------------------------*/

:- discontiguous(domain/2). % Klausel duerfen gemischt werden
:- discontiguous(interpretation/2). % Klausel duerfen gemischt werden

domain(i1, [a1, a2, a3, a4, a5, er]).
interpretation(i1,[peter -> a1, laura -> a3, junge -> [a1, a4],
                   haus -> [a5], winkt -> [a1, a2], lacht -> [a1, a3], 
                   rot -> [a5], sieht -> [(a1, a3), (a1, a5)],
                   es_regnet -> true, mutter -> [a1 -> a2, a2 -> er,
                                                 a3 -> a2, a4 -> a2,
                                                 a5 -> er, er -> er]]).

domain(i2, [a1, a2, a3, er]).
interpretation(i2,[peter -> a1, laura -> a2, junge -> [a1, a3],
                   haus -> [ ], winkt -> [a1, a2], lacht -> [a1, a3],
                   rot -> [ ], sieht -> [(a1, a2), (a3, a1), (a1, a1)],
                   es_regnet -> false, mutter -> [a1 -> a2, a2 -> er,
                                                  a3 -> a2, er -> er] ]).

domain(i3, [a1, a2, a3, a4, a5,er]).
interpretation(i3,[peter -> a4, laura -> a1, junge -> [a1, a3, a4],
                   haus -> [a5], winkt -> [a1, a2], lacht -> [a1, a3], 
                   rot -> [a5], sieht -> [(a1, a2), (a1, a5) ],
                   es_regnet -> false, mutter -> [a1 -> a2, a2 -> er,
                                                 a3 -> a2, a4 -> a2, 
                                                 a5 -> er, er -> er]]).

domain(i4, [a]).
interpretation(i4,[p1 -> [(a,a,a)], f -> [a -> a]]).

domain(i5, [a]).
interpretation(i5,[p1 -> [ ], f -> [a -> a]]).

domain(i6, [a,b,c]).
interpretation(i6,[p -> [ ]]).

domain(i7, [a,b,c]).
interpretation(i7,[p -> [(a,a), (b,b),(c,c),(a,b)]]).

domain(i8, [a,b,c]).
interpretation(i8,[p -> [(a,a), (b,b),(c,c),(a,b), (b,a), (a,c), (c,a)]]).

domain(i9, [a,b]).
interpretation(i9,[r -> [(a,a),(b,b)]]).

domain(i10, [a]).
interpretation(i10,[s -> [a],q -> [a]]).

domain(i11, [a,b]).
interpretation(i11,[s -> [b],q -> [a]]).
