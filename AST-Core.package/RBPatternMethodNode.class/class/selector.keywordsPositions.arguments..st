instance creation
selector: aSelector keywordsPositions: positionList arguments: valueNodes
	^(aSelector isPatternVariable) 
		ifTrue: [super selector: aSelector keywordsPositions: positionList arguments: valueNodes]
		ifFalse: 
			[RBMethodNode selector: aSelector keywordsPositions: positionList arguments: valueNodes]