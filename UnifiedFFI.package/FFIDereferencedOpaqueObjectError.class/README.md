I indicate that user tried to access an opaque type in a derreferenced way.
Opaque types can just be accessed by reference (as a pointer to them). 

Example:
self ffiCall: #( void function(FFIOpaqueObject  *var)  "Correct"

self ffiCall: #( void function(FFIOpaqueObject  var)  "WRONG"

