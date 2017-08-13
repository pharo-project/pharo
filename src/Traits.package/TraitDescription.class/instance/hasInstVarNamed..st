instance variables
hasInstVarNamed: aString
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self instVarNames includes: aString