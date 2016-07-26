initialize-release
selector: aSymbol keywordsPositions: positionList arguments: valueNodes 
	super selector: aSymbol keywordsPositions: positionList arguments: valueNodes.
	isList := (aSymbol keywords first value at: 2) == self listCharacter