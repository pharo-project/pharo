class variables
usesClassVarNamed: aString
	"Return whether the receiver or its superclasses have a class variable named: aString"
	
	^ self allClassVarNames includes: aString