Lab 4
=====

Instructions
------------

The file Lab4.pdf contains questions for the written part of the assignment, and details for the coding part of the assignment.  To finish the assignment, finish the code template in the file /src/main/scala/Lab4.scala.  You will need to modify the *typeInfer*, *step* and *substitute* functions, as well as finish the higher-order functions.  This lab is strongly typed, and so should behave differently in some cases than previous labs.

You are expected to write at least one javascripty test case.  Include this file in the directory *jsy_testcases*.  This test case should test for behavior more complex than the **sbt** test cases; very simple test cases (e.g., 1+2) will not receive a very high grade.  Included in *jsy_testcases* is an example test case.  Sample javascripty files are also available in the *example_jsy* folder.  Test cases which are just copies of these files or copies of the autotester test cases will receive no credit. 

For the written part of the assignment, please create a document called Lab4.md.  Include your name and identikey in the document!  Images can be included in any image format (.jpg, .png, etc.) as separate files, and refered to in the writeup.

### Deliverables

Your submission should have the following items included to be considered complete:

1.  Your interpreter should be implemented as *Lab4.scala* (simply modify the given template file in */src/main/scala/*.

2.  Your written portion should be included as *Lab4.md*.  Figures and illustrations can be included as image files and refered to in your writeup.  Parts 3c, 4 and 5 of the handout require a written answer.

3.  The *koans/* directory should contain the requested koans (see below) completed and passing all tests.

4.  The *jsy_testcases* directory should contain your javasripty test case(s).

5.  Fill out answers to the questions in *survey.md*.

6.  Submit your lab and test case to the autograder.	

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

### Readings

Suggested readings for this assignment are below.  Dr. Chang's notes are the most relevant for this assignment.  The Friedman and Wand chapters discuss scoping and binding of variables, if you are interested in more details.

-Dr. Chang's Course Notes - Chapter 3.3.3 covers type checking.

-Odersky, Spoon and Venners, *Programming in Scala*, Chapter 16.1 - 16.9 (Covers Lists), Chapter 17.2 (Covers Sets and Maps)

-Twitter Scala School - http://twitter.github.com/scala_school/collections.html

Further readings, if found, will be posted on Piazza.

### Koans

For this lab, finish the following koans:

-AboutHigherOrderFunctions
-AboutLists

Place completed koans in the directory *koans*.

### node.js

In this lab, we are varying slightly from javascript's behavior.  While node.js is useful for running your javascripty programs, you should not match its behavior.  Correctly running interpreters implement the behavior described in the operational semantics.

### jsy.js

The *jsy.js* file is a wrapper for *.jsy* files to run in node.js.  To run a javascripty file in node.js, use the following command

    $ nodejs jsy.js file.jsy

Note that your interpreter should behave differently than nodejs--your interpreter is now strongly typed, unlike Javascript. 

### survey.md

Please fill out the survey prior to submitting the assignment.  Make sure to write your solution between the square brackets (to make parsing easy for us).


### Github repository

This lab will deviate slightly from previous labs regarding pull requests.  You should issue a pull request by Wednesday, March 13th at 6:00 pm.  Name this pull request "Lab 4 - Work In Progress".  In the notes of the pull request, create a checklist (in markdown format) of things to do for the assignment.  You can check off these items as you complete them.  To create a checklist, fill out the in the comments markdown which is similar to the following:

    TODO:

    - [ ] This is how to make a checkbox in markdown

    - [ ] Checkboxes will be rendered as actual boxes you can check off. 

    - [x] Having an x in the checkbox indicates that it is already checked off.

Your pull request will be merged on the due date of the assignment.  There will be no need to issue a new pull request.


### Autotester

The online autotester for lab 4 will be available in a few days.  An anouncement will be made on Piazza when this tester is available.  As with the previous labs, you'll need to submit your assignment to the autotester at least one time.  We are working on linking the autotester to github, so that you won't need to use the web submission site in the future.  An anouncement on Piazza will be made when this is finished.


### Hints

Here's some common issues related to this lab:

1.  You will only need to edit Lab4.scala.  You do not need to include your identikey in the object or file name.

2.  The type checker should look very similar to big-step semantics.  The only major difference is that you do not perform the actual computations.  Type errors are thrown whenever two incompatible types are given.

3.  Higher order functions are *very* useful for dealing with argument / parameter lists and object fields.  Becoming familiar with these could save a lot of time when dealing with Function, Call, Object and GetField.

4.  Multi-argument function calls are not very different than single-argument function calls, you now have a list of substitutions rather than a single one.

5.  Strive for cake! 
