Lab 5
=====

Instructions
------------

The file Lab5.pdf contains questions for the written part of the assignment, and details for the coding part of the assignment.  To finish the assignment, finish the code template in the file /src/main/scala/Lab5.scala.  

You are expected to write at least one javascripty test case.  Include this file in the directory *jsy_testcases*.  This test case should test for behavior more complex than the **sbt** test cases; very simple test cases (e.g., 1+2) will not receive a very high grade.  Included in *jsy_testcases* is an example test case.  Sample javascripty files are also available in the *example_jsy* folder.  Test cases which are just copies of these files or copies of the autotester test cases will receive no credit. 

### Deliverables

Your submission should have the following items included to be considered complete:

1.  Your interpreter should be implemented as *Lab5.scala* (simply modify the given template file in */src/main/scala/*.

2.  The *jsy_testcases* directory should contain your javasripty test case(s).

3.  Fill out answers to the questions in *survey.md*.

4.  Submit your lab and test case to the autograder.	

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

In this lab, we are varying slightly from javascript's behavior.  While node.js is useful for running your javascripty programs, you should not match its behavior.  Correctly running interpreters implement the behavior described in the operational semantics.

### jsy.js

The *jsy.js* file is a wrapper for *.jsy* files to run in node.js.  To run a javascripty file in node.js, use the following command

    $ nodejs jsy.js file.jsy

Note that your interpreter should behave differently than nodejs--your interpreter is now strongly typed, unlike Javascript. 

### survey.md

Please fill out the survey prior to submitting the assignment.  Make sure to write your solution between the square brackets (to make parsing easy for us).

### Github repository

As with Lab 4, issue a WIP Pull request, with checkboxes containing the tasks remaining to be performed.  Please have this pull request issued by Wednesday, April 10th.

### Autotester

The online autotester for lab 5 will be available in a few days.  An anouncement will be made on Piazza when this tester is available.  As with the previous labs, you'll need to submit your assignment to the autotester at least one time.  We are working on linking the autotester to github, so that you won't need to use the web submission site in the future.  An anouncement on Piazza will be made when this is finished.


Strive for cake!