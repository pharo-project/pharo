testing
hasTempVector: aSymbol
	^ (blockSequence hasTempVector: aSymbol) or:[sequence hasTempVector: aSymbol]