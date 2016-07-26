class variables
allClassVariables
	"Answer a collection of the receiver's classVariables, including those defined its superclasses"

	^self hasSuperclass 
		ifFalse:[ classVariables ]
		ifTrue:[ self superclass allClassVariables, classVariables ]