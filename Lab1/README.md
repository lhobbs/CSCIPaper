Lab 1
=
Lisa Hobbs and Daniel Morrissey


Problem 1
-
The use of pi at line #4 is bound by the declaration at line 3 because it is inside the definition and inside the definition pi is redeclared. The use of pi at line #7 is bound by the declaration at line #1 because it cannot recognize the declaration inside the definition because it is being used outside the definition.

Problem 2
-
The use of x at line #3 is bound by the declaration at line #2 because it is being used as a variable passed in as an argument in the definition. The use of x at line 6 is also bound by the same declaration because it is still inside the definition. Same with the use at line #10 because it is outside of where the new val is declared. The use of x at line 13 is bound by the declaration at line #1 because it is outside the definition.

Problem 3
-
The return type of the function g is ((Int, Int), Int).

G : ((Int, Int) Int) because <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
(b, a + 2) : ((Int, Int) Int) because <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
b : (Int, Int) <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
a : (Int) <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
(b, 1) : ((Int, Int), Int) because <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
b : (Int, Int) <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
1 : (Int)




