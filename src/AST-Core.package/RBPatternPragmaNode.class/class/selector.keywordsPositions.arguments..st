instance creation
selector: aSelector keywordsPositions: positionList arguments: valueNodes
	^(aSelector isPatternVariable)
		ifTrue: [ super selector: aSelector keywordsPositions: positionList arguments: valueNodes ]
		ifFalse: [ RBPragmaNode selector: aSelector keywordsPositions: positionList arguments: valueNodes ]