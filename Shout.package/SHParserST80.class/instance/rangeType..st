recording ranges
rangeType: aSymbol 

	self failWhen: currentToken isNil.
	^ self
		rangeType: aSymbol
		start: currentTokenSourcePosition
		end: currentTokenSourcePosition + currentToken size - 1