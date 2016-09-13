I am the baseclass for a "class definition change", a change that will create a class or modify the definition of an existing class.
I hold the class definition string and a controller used by the compiler when compiling the class definition for notifying about the
compilation result. My subclass define the kind of class definition I can add, a class, a metaclass a trait or a class trait.

My instance variable "definedClass" is the new class object.