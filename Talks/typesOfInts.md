#PEP 237#
Unifying Long Integers and Integers in Python

Principals of Programing Languages

Lisa Hobbs
Daniel Morrissey 



#Overview#

- Like most languages, Python began with both a short int and a long int type
- Starting in version 2.2, they began the implementation of combing these types
- The new type int would behave like the long int did previously
	- the size of this type is limited only by the hardware of the running system
	
#Implementation#

- Began in version 2.2
- In version 2.3, Python would throw FutureWarning when an integer was used in a way that would change later on
- By version 2.4, all uses of integers were the type of a long int

#Pros#
- This unification simplified things by only having one type
- It helped users not have to remember the ranges of each type
- It also helps beginner programmers learn the language faster by not having to worry about types of integers

#Cons#
- Python is built on C, which still uses multiple precisions

#Useful Places of Having One Type#
- Reduces occurances of overflow errors
- Reduces errors in arithmetic operations
- Increases accuracy of left shifts

#Slide 6#

#Slide 7#

#Slide 8#

#Slide 9#

#Slide 10#

#Slide 11#

#Slide 12#

#Slide 13#

#Slide 14#

#Slide 15#

#Slide 16#

#Slide 17#

#Slide 18#

#Slide 19#

#Any Questions? 

Thanks! 

#Other Notes#

to make this, run this:

./generate genericsPresentation.md 

to run the pdf with 5 min timer:

pdf_presenter_console genericsPresentation.pdf --duration=5







