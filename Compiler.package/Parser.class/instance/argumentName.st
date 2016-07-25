expression types
argumentName

	hereType == #word
		ifFalse: [^self expected: 'Argument name'].
	^self advance