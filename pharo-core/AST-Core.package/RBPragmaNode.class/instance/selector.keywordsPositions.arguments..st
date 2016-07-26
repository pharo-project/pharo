initialization
selector: aSymbol keywordsPositions: positionList arguments: valueNodes
	aSymbol numArgs == valueNodes size
		ifFalse: 
			[self error: 'Attempting to assign selector with wrong number of arguments.'].

	self 
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList.