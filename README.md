# Parser 1
A monadic parser that implements the grammar below and parses lists of equations.

Grammar:
S ::= Q , S
  ::= Q
Q ::= E = E
E ::= T E0
E0 ::= + T E0
   ::= - T E0
   ::= \epsilon
T  ::= F T0
T0 ::= * F T0
   ::= / F T0
   ::= "
F  ::= id
   ::= num
   ::= - F
   ::= ( E )
