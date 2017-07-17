instance variables
removeInstVarNamed: aString 
	"Remove the argument, aString, as one of the receiver's instance 
	variables. Create an error notification if the argument is not found."

	^self subclassResponsibility