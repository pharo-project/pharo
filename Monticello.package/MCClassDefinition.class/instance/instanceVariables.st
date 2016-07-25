printing
instanceVariables
	^self usesSpecialVariables 
		ifTrue: [self variablesOfType: #isInstanceVariable]
		ifFalse: [self instanceVariablesString asSlotCollection]