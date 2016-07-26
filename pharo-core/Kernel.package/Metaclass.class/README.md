My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance.
	
[Subtle] In general, the superclass hierarchy for metaclasses parallels that for classes. Thus,
	Integer superclass == Number, and
	Integer class superclass == Number class.
However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus,
	Object superclass == nil, and
	Object class superclass == Class.

[Subtle detail] A class is know by name to an environment.  Typically this is the SystemDictionary named Smalltalk.  If we ever make lightweight classes that are not in Smalltalk, they must be in some environment.  Specifically, the code that sets 'wasPresent' in name:inEnvironment:subclassOf:instanceVariableNames:variable:words:pointers:classVariableNames:poolDictionaries:category:comment:changed: must continue to work.