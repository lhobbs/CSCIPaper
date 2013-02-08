Lab 2
=====

Instructions
------------

The file Lab2.pdf contains questions for the written part of the assignment, and details for the coding part of the assignment.  To finish the assignment, finish the code template in the file /src/main/scala/Lab2.scala.  You should only need to add additional case statements in the eval function.  There should be no need to alter the remaining part of the assignment.

For the written part of the assignment, please create a document called Lab2.md.  Include your name and identikey in the document!  Images for parse trees and judgements can be included in any image format (.jpg, .png, etc.) as separate files, and refered to in the writeup.

### sbt

For your convenience, an sbt script (build.sbt is included).  You can issue the following commands to compile, run and test your code:

    $ sbt compile 

compile your program

    $ sbt clean

delete the previous compilation

    $ sbt "run --debug file.jsy"   

runs the interpreter in debug mode with file.jsy as input.  The optional --debug flag provides informative messages throughout interpretation of the input file.

    $ sbt test

executes the test suite.  Note that the test suite is designed to test the function of individual terms, and not necessarily the interaction between.  The online autotester will contain more complex test cases. 

sbt is available at http://www.scala-sbt.org.  


### node.js

As mentioned in the handout, the semantic of this interpreter is supposed to match those of node.js.  Node.js is available at http://nodejs.org.  Note that javascript is not strongly typed, and how a particular combination of values are evaluated may not seem intuitive.  Node.js should be used to determine how these cases are handled.


### survey.md

Please fill out the survey prior to submitting the assignment.  Make sure to write your solution between the square brackets (to make parsing easy for us).


### github repository

Once you are finished with the assignment, push your local copy to your github account, and issue a pull request.


### Autotester

The online autotester will be available in a few days.  An announcement with url will be made on Piazza when this occurs.  As with Lab 1, you'll need to submit your assignment to the autotester at least one time.


### Hints

Here's some common issues related to this lab:

1.  You will only need to edit Lab2.scala.  You do not need to include your identikey in the object or file name.

2.  the eval function returns Expr objects.  Keep in mind that there are cases for values--numbers are represented by the class N, and booleans are represented by the class B.  If you are getting errors, check to make sure that you are wrapping the actual number / boolean values with the correct class.

3.  The file ast.scala contains a list of all possible terms in the abstract syntax tree. 

