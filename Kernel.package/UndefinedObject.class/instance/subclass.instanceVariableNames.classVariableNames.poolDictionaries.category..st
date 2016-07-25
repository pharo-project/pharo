class hierarchy
subclass: nameOfClass  
	instanceVariableNames: instVarNames
	classVariableNames: classVarNames
	poolDictionaries: poolDictnames
	category: category
	"Calling this method is now considered an accident.  If you really want to create a class with a nil superclass, then create the class and then set the superclass using #superclass:"
	self traceCr: ('Attempt to create ', nameOfClass, ' as a subclass of nil.  Possibly a class is being loaded before its superclass.').
	^ Object
		subclass: nameOfClass
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: poolDictnames
		category: category
