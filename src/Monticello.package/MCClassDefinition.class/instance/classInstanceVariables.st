printing
classInstanceVariables
	^self usesSpecialVariables 
		ifTrue: [ self variablesOfType: #isClassInstanceVariable]
		ifFalse: [self classInstanceVariablesString asSlotCollection]