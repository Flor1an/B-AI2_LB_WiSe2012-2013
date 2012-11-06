/**********************************************************************

         name: examples.pl
      version: 06.11.2012
  description: Example sequences for propostional logic
      authors: Carola Eschenbach (CE)
               Christoph Klauck
     enhanced: FK
               MS

**********************************************************************/

:- module(examples,[aussagensymbol/1, 
                    example_F/2 
                   ]).
  
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
:- op(750, fy,user:(~)).         % negation
:- op(800,yfx,user:($)).         % NEU NAND

/*-----------------------------------------------------------------------------

Vokabular der frei waehlbaren Symbole
hier koennen die Beispieldateien beliebig variieren

Aussagenlogik: nur aussagensymbol
-----------------------------------------------------------------------------*/

aussagensymbol(a).
aussagensymbol(b).
aussagensymbol(c).
aussagensymbol(d).
aussagensymbol(e).
aussagensymbol(f).
aussagensymbol(g).
aussagensymbol(h).
aussagensymbol(m).
aussagensymbol(p).
aussagensymbol(q).
aussagensymbol(s).
aussagensymbol(p1).
aussagensymbol(p2).
aussagensymbol(p3).
aussagensymbol(aw).
aussagensymbol(bw).
aussagensymbol(cw).


% Aufgabe 2 Seite 90 LBSCRIPT.pdf     INFO: x = a; y = b; z = c

example_F(b(1), (   (a > b) <> ((a & ~b) > b)    )).
example_F(b(2), (   (a > b) > ((c v a) > (c v b))    )).
example_F(b(3), (   ((a > b) > (b > c)) > (a > c)    )).
/* Warum wurde die Aufgabe gewählt?: Wir haben diese Aufgabe genommen, da sie
uns als guter Einstieg schien, da wir das Ergebniss gut per Hand nachverfolgen
können und somit überprüfen können ob ProLog sie richtig interpretiert.  */



/* Beispiel-Formeln
*/
example_F(f(1), (p $ q)).
example_F(f(2), ((p & q) v (~ p v ~ q))).
example_F(f(3), ~ (a & (~ b) & c & (~ d) & e & (~ f) & g)).
example_F(f(4), ~ ((a v b v d) & (~ b v ~ c v ~e) & (c v d v f) &
                 (~d v ~e v ~ g) & (e v f v ~ a) & (~ f v ~ g v ~b) &
                 (g v ~ a v ~ c))).
example_F(f(5), ~ ((a v b) & (~ b v ~ c) & (c v d) &
                 (~d v ~e ) & (e v f) & (~ f v ~ g) &
                 (g v ~ a))).

example_F(a(1), a > b).
example_F(a(2), b > c).
example_F(a(3), ~ c).
example_F(a(4), (a > b) <> ((a & (~b)) > b)).
example_F(a(5), (a > b)>((c v a) > (c v b))).
example_F(a(6), (a > b)>((b > c) > (a > c))).
example_F(a(7), ((a v ~a) & (b v ~a) & (~c v ~a) & (a v (~b) v c) & (b v (~b) v c) & (~c v (~b) v c) & (b v a))).
example_F(a(8), ((p v ~p) & (~s v ~p) & (p v s) & (~s v s) & (~s v a) & (~a v p) & p & s)).
example_F(a(9), (~c v a) & (~c v b) & (~a v (~e) v d)& ((~b) v (~d) v f)).
example_F(a(10), (c & e & (~f))).
example_F(a(11), (~c v a) & (~c v b) & (~a v (~e) v d)& ((~b) v (~d) v f) & c & e & (~f)).

example_F(t(1), a > a).
example_F(t(2), (a>b)>((b>c)>(a>c))).
example_F(t(3), (a v b) <> ~(~a & ~ b)).
example_F(t(4), (a > b) <> (~a v b)).
example_F(t(5), (a v b) <> (b v a)).
example_F(t(6), (a v (b & c)) > ((a v b) & (a v c))).
example_F(t(7), (a & b) <> (~(a > (~ b)))).
example_F(t(8), (a v b) <> ((~a) > b)).

example_F(t(hilbert1), a > (b > a)).
example_F(t(hilbert2), (a > (b > c)) > ((a > b) > (a > c))).
example_F(t(hilbert3), ((~ b) > (~ a))> (a > b)).

example_F(t(meredith), ((((a>b)>((~c) > (~d)))>c)>e)>((e>a)>(d>a))).

/*
Wenn Vater Meier kommt, dann auch Mutter Meier.
Mindestens eines der Kinder kommt mit.
Aber wenn Mutter Meier kommt, dann kommt Grete nicht.
Wenn Hans kommt, dann auch Grete und Vater Meier.

aussagenlogische Variablen: p Vater Meier kommt, m Mutter Meiner kommt,
                            h Hans kommt, g Grete kommt
*/
example_F(a(12), (p > m) & (h v g) & (m > (~g)) & (h > (p & g))).
% kommt Grete?
example_F(a(13), ((p > m) & (h v g) & (m > (~g)) & (h > (p & g))) & (~g)).
example_F(a(14), ~(((p > m) & (h v g) & (m > (~g)) & (h > (p & g))) > g)).
example_F(a(15), (((p > m) & (h v g) & (m > (~g)) & (h > (p & g))) > g)).
example_F(a(16), (aw <> cw)).
example_F(a(17), (bw <> ((~aw) > bw))).
example_F(a(18), (cw <> ((~bw) > (~aw)))).
example_F(a(19), ((aw <> cw) & (bw <> ((~aw) > bw))) & (cw <> ((~bw) > (~aw)))).
example_F(a(20), (((aw <> cw) & (bw <> ((~aw) > bw))) & (cw <> ((~bw) > (~aw)))) & (~ cw)).
example_F(a(21), (bw <> (aw <> ~aw))).
example_F(a(22), (cw <> (~ bw & aw))).
example_F(a(23), (bw <> (aw <> ~aw)) & (cw <> (~ bw & aw))).
example_F(a(24), ((bw <> (aw <> ~aw)) & (cw <> (~ bw & aw))) & bw).
example_F(a(25),( ((~ (~ b)) v a) & (((~ a) & (~ (b & (~ c)))) v (a & (b & (~ c)))))). % Ausgangsformel
example_F(a(26),(b v a) & (((~ a) & ((~ b) v c)) v (a & (b & (~ c)))) ). % NNF
example_F(a(27),(a v b) & ((~ a) v a) & (a v (~ b) v c) & ((~ a) v b) & ((~ a) v (~ c)) & ((~ b) v b v c) & ((~ b) v c v (~ c))). % KNF
example_F(a(28), (a v c) & ((~ a) v  (~c)) & b). % optimierte KNF
example_F(a(29), (b & (~ a) & (~ b)) v (a & (~ a) & (~ b)) v (b & (~ a) & c) v (a & (~ a) & c) v (b & a & b & (~ c)) v (a & a & b & (~ c))). % resultierende DNF
example_F(a(30), ((~ a) & b & c) v (a & b & (~ c))). % optimierte DNF
example_F(a(31), (((~ a) > b) <> ((a > b) > b))).

