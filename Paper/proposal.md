	In the spring of 2002 there was a proposal to add a type bool to Python.  This proposal adds constants True and
	False that behave as 1 and 0 in most situations.  There were some controversies over implementing this type, but
	in the end it was approved and added to the Python language.  Guido van Rossum explains that most languages 
	eventually implement a Boolean type because many programmers desire to have true and false values.  In many 
	modules prior to this implementation the constants �True� and �False� are defined due to this need from the 
	programmers perspective.  Having everyone define their own Boolean constants can cause confusion because they are 
	not all defined the same.  The naming conventions vary across the different modules including �FALSE�, �
	false�, �False�, �F� or �f�.  This makes it difficult to program with because the programmer must remember the 
	convention for each individual module.  Having a standard constant eliminates this confusion.  Another
	difficulty with these constants were their exact values.  Sometimes false would be set to �0� and other times 
	it would be �None�.  Again, having a standard type resolves this issue.  
	Some of the controversies included return values, constant names, arithmetic operations with Booleans, and
	inherency.  Most reviewers agreed that str(True) should return True instead of 1.  In the debate about the
	return type of operator.truth(x) most agreed  that it should return a bool.  However, Tim Peters believed
	it should return an int.  Guido van Rossum, the author of this PEP, rationalized that the purpose of this
	operation is to force a Boolean context on its argument and therefore since the Boolean type exists, it should
	be used.  When it came to naming the constants, some believed that they should be referred to as �true� and 
	�false� as in many other languages such as C++ and Java.  However, most reviewers believed that they should 
	be called �True� and �False� because other constants in Python are named in this convention such as �None�.   It
	was decided that arithimatic operations with type bools would be allowed, although some reviewers believe that
	�textbook� bools should be implemented and these operations would be illegal.  To make some of these arithmetic 
	operations work, bool is inherited from int.  
