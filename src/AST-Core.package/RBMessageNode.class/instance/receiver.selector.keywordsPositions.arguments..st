initialize-release
receiver: aValueNode selector: aSelector keywordsPositions: positionList arguments: valueNodes
	aSelector numArgs == valueNodes size
		ifFalse: 
			[self error: 'Attempting to assign selector with wrong number of arguments.'].

	self 
		receiver: aValueNode;
		arguments: valueNodes;
		selector: aSelector;
		keywordsPositions: positionList.