instance variables
removeInstVarNamed: aString 
	"Remove the argument, aString, as one of the receiver's instance variables."

	^self removeSlot: (self slotNamed: aString)