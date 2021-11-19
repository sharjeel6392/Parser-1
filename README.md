# Parser 1
A monadic parser that implements the grammar below and parses lists of equations.<br />

Grammar:<br />
S &nbsp::= Q , S <br />
&emsp;::= Q<br />
Q &nbsp::= E = E<br />
E &nbsp::= T E0<br />
E0 &nbsp::= + T E0<br />
&emsp;::= - T E0<br />
&emsp;::= \epsilon<br />
T &nbsp::= F T0<br />
T0 &nbsp::= * F T0<br />
&emsp;::= / F T0<br />
&emsp;::= "<br />
F  &nbsp::= id<br />
&emsp;::= num<br />
&emsp;::= - F<br />
&emsp;::= ( E )<br />
