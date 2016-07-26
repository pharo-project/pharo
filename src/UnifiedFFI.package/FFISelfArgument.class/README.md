I represent a "self" argument. 
This means a call on the form: 

#(void function ( self ) )

To be able to pass an object as "self " it need to accomplish one of this conditions: 

- it has to be an object mappable to an atomic type (that means: a number, a char, a string, ...)
- it class needs to have an instance variable 'handle', who will be taken as the real value to transmit (usually, this will be a en ExternalAddress)