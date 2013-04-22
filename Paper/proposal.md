#Intro
Daniel Morrissey and Lisa Hobbs

Prior to release version 2.2.2, Python differentiated integer types as most languages do, IE with an int, a short int and a long type. Starting in 2.2.2, as outlined by Python Enhancement Proposal number 237, these three types were reimplimented as one single type, int. This unified type appears to the user as an arbitrary precision integer, limited in size only by the hardware of the running system (this was the previous functionality of the long type pre 2.4). This change diminishes the overhead and errors that can occur by integer overflow and mixed type aritmetic. The change also extends the convenience of python, which already hid the memory relations for types such as strings.
    
    1) Implementation
        A) Phases
        B) 2 Possible implementations
            A) an actual union
            B) short and long types still exist, common superclass integer
        C) Obstacles
            A) Underlying C code still has to be aware of short vs long
            B) 
    1) Error fixes
        A) Short, long and int problems
        B) Vs 32bit and 64bit numbers
        C) Portability
    2) Convenience
        A) User no longer needs to worry about compatibilities, simple
        



#Points: 
        - Simplification on user end
        - No need for multiple types for extending integer precision
        - Dependant only on hardware
        
        
#Pros
        - One type for integers
        
        
#Cons/Obstacles
        - Underlying c code must still use multiple precisions
        
        
        
#Versions: 
        - Implementation begun in 2.2
        
##Links:
http://mail.python.org/pipermail/python-3000/2006-August/003046.html
http://www.osdata.com/programming/datatypes/arbitraryprecisionintegers.html
http://docs.python.org/release/2.4.4/whatsnew/node3.html
http://docs.python.org/release/2.2.1/whatsnew/node6.html


#Lisa's Comments:
We should elaborate on the implementation, especially in including something about Python 2.3 because they state that the 
implementation began in 2.2 and then it was final in 2.4, so we should talk about the transition in 2.3.  We should try
to find some user input to see if the implementation was done well or if it made people go through extra work to debug
old code to have it be workable with newer versions of the language.  This could determine if there are more things we 
should add to the cons.  For pros we could include how it makes it easier for a new programmer to learn because they
don't have to remember the different ranges of numbers.  
