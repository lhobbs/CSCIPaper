Lab 2 - Lisa Hobbs
==

1. Survey
-
2. Grammars: Synthetic Examples
-
A) Lab2Supplement <br>
B) A ::= A & A | V <br>
C) This grammar defines a string of any length consisting of all a's, b's, or c's, or the empty set. <br>
D) 1, 2, and 4 can all be derived using the grammar. Shown in Lab2Supplement.<br>
E) 1, 2, 3, and 5 can all be derived using the grammar. Shown in Lab2Supplement.<br>

3. Grammars: Understanding a Language
-
A) The first grammar defines a sentance of at least one opperand preceded by 0 or more operators and additional opperands. The second one defines a sentace of at least one opperand followed by 0 or more operators and additional opperands. The two grammars are not the same, one starts and builds left, the other starts and builds right IE. 2 * 8 + 4 vs 4 + 8 * 2

B) The first grammar describes at least one number, id, or a sequence of numbers or id's combined by operators IE. 5 or 5 - red. The second grammar describes at least one number or id, in addition to 0 or more opperator suffixes. The grammars can be used to describe identical sentances.

C) The + sign is left associative, and has a higher precedence than >. In the example 7 + 1 > 3, the additon of 7 + 1 is evaluated first, and then this result will be compared to 3.

D) In scala, - has higher precedence than <<. This can be shown by the two expressions 2 - 1 << 2 and 2 << 1 - 1. In the first expression, scala evaluates to 2, which shows precedence of the (-). To ensure it wasn't assosiativity causing this result, we tried the second expression, which also evaluated to 2. If << took precedence, the expression results would be 0 and 3 respectively.

E) float ::= Number . Number E Number

