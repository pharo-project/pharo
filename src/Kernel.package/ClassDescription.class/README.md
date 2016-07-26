I add a number of facilities to basic Behaviors:
	Named instance variables
	Category organization for methods
	The notion of a name of this class (implemented as subclass responsibility)
	The maintenance of a ChangeSet, and logging changes on a file
	Most of the mechanism for fileOut.
	
I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.