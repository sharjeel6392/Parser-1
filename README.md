# Parser 1
A monadic parser that implements the grammar below and parses lists of equations.<br />

Grammar:<br />
S ::= Q , S <br />
  ::= Q<br />
Q ::= E = E<br />
E ::= T E0<br />
E0 ::= + T E0<br />
   ::= - T E0<br />
   ::= \epsilon<br />
T  ::= F T0<br />
T0 ::= * F T0<br />
   ::= / F T0<br />
   ::= "<br />
F  ::= id<br />
   ::= num<br />
   ::= - F<br />
   ::= ( E )<br />
