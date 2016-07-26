printing
classVariables
	^self usesSpecialVariables 
		ifTrue: [self sortedVariablesOfType: #isClassVariable ]
		ifFalse: [(self classVariablesString substrings: ' ') collect: [:x | x asSymbol => ClassVariable]].
