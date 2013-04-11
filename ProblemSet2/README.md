
Overview
========

In this problem set you will focus again on how to use and implement higher-order functions in Scala. You may consult any internet resource or your instructors. However, you may not collaborate on these problems with other students, except to clarify the questions here. (Piazza is a very good place to ask such questions.) The problem set has a firm deadline of Thursday April 11 at 7 pm. Remember, partial credit will **not** be given for late submissions.

Start first by looking at the code we have provided you. Your Scala solutions should implement both the tests and the corresponding code in this directory. Please use the standard `sbt` commands, especially `sbt test`.

Useful resources include the [S99 problem site][s99], the [lecture notes][lectures], and the [Programming in Scala 2e][PiS2e] book.


Rewrite nested loops join
=========================

The following for-expression is one version of writed a nested-loops join, and it's also the requested solution to the corresponding problem on Problem Set #1. Implicitly this for-expression takes advantage of monadic composition on Lists, where `List(x)` implements *unit* and `List.flatMap` implements *bind*:

	def join[A,B](xs: List[A], ys: List[B], p: (A, B) => Boolean):List[(A,B)] = {
		for (x <- xs; y <- ys if p(x, y)) yield (x, y)
	}

Rewrite `join` in terms of `map`, `flatMap`, and `withFilter`. Apply the translation scheme discussed in class and in greater detail in Section 23.4 of *Programming in Scala 2ed*.


SAT
===

The [SAT][] in this problem is not what was formerly known as the Scholastic Assessment Test; instead, it's a classic problem in computer science to determine whether an assignment of variables for a Boolean expression *exists* such that the expression evaluates to being true. (There exist other equivalent terms for this problem setting, such as statements in propositional logic.)

Here's one possible grammar for the describing Boolean expressions, where we write out `and`, `or`, `not` as keywords; a literal may be `true`, `false`, or a variable such as `x1`:

	expr ::= clause { and clause }
	clause ::= term { or term }
	term ::= literal | not expr
	
Parentheses are assumed.


Part 1
------

Assume the following defined types:

~~~~~
type Env = Map[String, Boolean]

sealed abstract class Expr
case class B(b: Boolean) extends Expr
case class Var(x: String) extends Expr
case class Not(e1: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
~~~~~

In particular, some of these classes should look awfully familiar to you from the labs. The first part asks you to be a human parser: to translate various Boolean expressions into an expression tree of `Expr` nodes:

* E0 = true and not false or x
* E1 = x1 and x2 and x3
* E2 = (not x1 or not x2 or not x3) and (x1 or x2 or x4)
* E3 = not x or y
* E4 = x and (y or (z and w))

Replace E0-E4 in ProblemSet.scala with the correct expressions.

Extra credit. Write a parser using Scala's parser combinator support to parse a given Boolean expression.


Part 2
------

2.1 Write a function `eval` that given an `Env` assigning variables to their Boolean values and a Boolean expression, evaluates it as `true` or `false`.

2.2 Write a function `satisfiable` that given a Boolean expression can determine if there exists a satisfying assignment for names - either true or false - such that the expression can be evaluated to be true.

Extra credit. Write a function `normalize` that rewrites a given Boolean expressions in conjunctive normal form.



[lectures]: https://github.com/csci3155/notes/tree/master/lectures
[PiS2e]: http://www.artima.com/shop/programming_in_scala_2ed
[s99]: http://aperiodic.net/phil/scala/s-99/
[SAT]: http://en.wikipedia.org/wiki/Boolean_satisfiability_problem
