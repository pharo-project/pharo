initialization
selector: aSymbol keywordsPositions: positionList arguments: valueNodes
	self 
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList.
	isList := (aSymbol keywords first value at: 2) == self listCharacter