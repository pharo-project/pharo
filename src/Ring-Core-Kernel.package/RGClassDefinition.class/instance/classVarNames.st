class variables
classVarNames
	"Answer a collection of the names of the class variables defined in the receiver."
	
	^classVariables collect:[ :cvar| cvar name ]