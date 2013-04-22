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
        B) 



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
