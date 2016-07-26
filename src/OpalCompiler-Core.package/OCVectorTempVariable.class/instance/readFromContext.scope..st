debugging
readFromContext: aContext scope: contextScope

	| offset pairVariableVector |
	pairVariableVector := contextScope lookupTempVector: vectorName inContext: aContext.
	offset := pairVariableVector first indexInTempVectorFromIR: name.
	^pairVariableVector second at: offset.