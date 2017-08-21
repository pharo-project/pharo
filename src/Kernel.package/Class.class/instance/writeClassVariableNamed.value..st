class variables
writeClassVariableNamed: aString value: anObject
	"Store anObject in the class variable."
	
	(self classVariableNamed: aString) write: anObject