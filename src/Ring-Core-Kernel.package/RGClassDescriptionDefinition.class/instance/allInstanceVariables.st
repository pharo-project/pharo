instance variables
allInstanceVariables
	"Answer a collection of the receiver's instanceVariables, including those defined in its superclass"

	^self hasSuperclass 
		ifFalse:[ instanceVariables ]
		ifTrue:[ self superclass allInstanceVariables, instanceVariables ]