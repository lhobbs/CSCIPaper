Overview
========

In this problem set you will focus on how to use and implement higher-order functions in both Scala and JavaScript. You may consult any internet resource or your instructors. However, you may not collaborate on these problems with other students, except to clarify the questions here. (Piazza is a good place to ask such questions.) The problem set has a firm deadline of Thursday March 7 at 7 pm. Partial credit will not be given for late submissions.

Your Scala solutions should implement both the tests and the corresponding code in this directory. Please use the standard `sbt` commands, especially `sbt test`. See below for more on the JavaScript portion.

Useful resources include the [S99 problem site][s99], the [lecture notes][lectures], and the Programming in Scala 2e book.


P16 (**) Drop every Nth element from a list
===========================================

Example:

    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == 
           List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
	
Several solutions are provided on the S99 site. However, your solution must use a for-expression.


P19 (**) Rotate a list N places to the left
===========================================

Examples:

    assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == 
           List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == 
           List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
	
Unlike the solution provided, your solution must use `span`.


Nested loops join
=================

The nested loops join is a general mechanism for implementing the join seen in relational databases. Your join will combine two lists `xs` and `ys` such that a tuple `(x, y)` is generated if the predicate `p(x, y)` is true for each `x` in `xs` and each `y` in `ys`. Hint: use nested for expressions to combine. Your solution should be just a few lines if correct.

See the join function for the specific API and the corresponding tests for some examples of how it should work.


Euclidean algorithm in JavaScript
=================================

The [Euclidean algorithm][euclidean] finds the greatest common divisor; it can be expressed in Scala as follows:

    def gcd(a: Int, b: Int): Int = {
        def _gcd(m: Int, n: Int): Int = n match {
            case 0 => m
            case _ => _gcd(n, m % n)
        }
        _gcd(a max b, a min b)
    }

Write a recursive solution in JavaScript that matches this functionality. Modify `ProblemSet1.js` accordingly. You can use `Math.min` and `Math.max` in your solution. Verify your solution works by using nodejs:

    $ nodejs ProblemSet1.js
	
This skeleton code uses the assert module (included in nodejs) to verify your code.


P10 (*) Run-length encoding of a list
=====================================

The P10 problem is that consecutive duplicates of elements are encoded as sublists `[N, E]` where `N` is the number of duplicates of the element `E`. Note that this problem relies on a few other problems in its solution. Simply work through each one and translate to JavaScript. In particular, to translate `head` and `tail` of `xs` this would be `xs[0]` and `xs.slice(1, xs.length)` respectively. To concatenate two lists, `xs.concat(ys)` will work.

With that in mind, implement this function:

    function encode(xs) { throw "not implemented"; }

Such that these tests will

    assert.deepEqual(encode([]), [])
    assert.deepEqual(encode(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]),
		             [[4, "a"], [1, "b"], [2, "c"], [2, "a"], [1, "d"], [4, "e"]])



[euclidean]: http://en.wikipedia.org/wiki/Euclidean_algorithm
[lectures]: https://github.com/csci3155/notes/tree/master/lectures
[s99]: http://aperiodic.net/phil/scala/s-99/
