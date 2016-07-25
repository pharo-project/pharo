private
validateSelector: aSymbol
	(aSymbol numArgs between: 0 and: 2) ifFalse:[^self error: 'Invalid pluggable selector'].