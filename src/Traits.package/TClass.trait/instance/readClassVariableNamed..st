class variables
readClassVariableNamed: aString
	"Answer the content of the Class Variable"

	^(self classVariableNamed: aString) read