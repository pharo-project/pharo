class variables
removeClassVarNamed: aString
	"Remove the class variable whose name is the argument, aString, from 
    the names defined in the receiver, a class. Create an error notification if 
    aString is not a class variable or if it is still being used in the code of 
    the class."
	
	self removeClassVarNamed: aString interactive: false