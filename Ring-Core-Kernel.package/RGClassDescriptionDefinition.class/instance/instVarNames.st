instance variables
instVarNames
	"Answer a collection of the names of the instance variables defined in the receiver."
	
	^instanceVariables collect:[ :ivar| ivar name ]