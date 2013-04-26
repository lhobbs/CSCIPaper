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


#Implementation:
The transition process for unifying long integers and integers begun in Python 2.2.. During Python 2.3, a FutureWarning
would appear during operations that would produce different results after the integer unification.  The returned value 
would be limited to 32 or 64 bits, depending on the user’s platform.  These warnings went away in 2.4 and instead 
returned a long integer.  T  In Python 2.3 would trigger a warning on a 32 bit machine if a user tried an expression 
such as 2 << 32.  This would evaluate to 0.  However, in Python 2.4 this expression would return 8589934592, which is 
the correct answer. Performing left shifts and long hexadecimal and octal constants were problematic prior to this
unification.  Performing a left shift on a short could cause the result to lose bits.  Now a long is returned which 
contains all of the shifted-out bits and giving the correct value.  In previous versions of Python, arithmetic 
operations would often result in an OverflowError when performed on shorts.  Now it correctly performs the operations
and returns a long integer. 
