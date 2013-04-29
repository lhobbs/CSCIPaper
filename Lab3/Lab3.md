Lab 3 
=
###
4) Evaluation Order
####
a) 
Scala calls the function before evaluating its argument. This can be shown by defining a function as: 
<code>
	def fib () = { println("World"); (x:Int) => { x + 1; }}
</code>

and a second function as: 
<code>def func = { println"Hello"; 3; }</code>.

When <code>fib()(func)</code> is called, the output is "World" then "Hello" with a result of 4. This means that fib was called ("World"), and then func was evaluated ("Hello").

Scala evaluates arguments from left to right. Say we have the following functions defined:

<code>
foo() = { (x:Int, y:Int) => { x + y} } <br>
bar() = { println("arg1"); 2; } <br>
boo() = { println("arg2"); 3; }
</code>

When Calling foo()(bar, boo), the output is "arg1, arg2", meaning bar is evaluated before boo, meaning its from left to right.
####
b)
The evaluation order for e1 + e2 is

###
5) Short-Circuit Evalutation
####
a) 
A real life example of the usefulness of short circuiting is state checking. I was working on a project where we had a line similar to:

<code>if(isFollowing && ((valx - valy) < distance)) { … }</code>

The usefulness here is that we can skip the calculations if isFollowing is false. This can improve efficiency in things like GameLoops.

#####
b) 
Scala does use short-circuit evaluation. We can test this by defining a function as:
 
<code>def tr : Boolean = {
println("test"); true; }</code>

and then calling it as such: 

<code>if(false && tr) println("Done");</code>

When called like this, there is no output, meaning tr() isn't called, and the if statement evaluates to false.

####
c) 
Javascripty does use short circuiting, and e1 && e2 will short circuit. This is shown in the judgment for DoAndFalse from figure 7. The statement v1 && e2 is said to be false if v1 is false, not taking into consideration the value of e2. This is exactly what short circuiting is.
