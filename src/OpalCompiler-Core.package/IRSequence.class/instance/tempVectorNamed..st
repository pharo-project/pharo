accessing
tempVectorNamed: aSymbol
	^sequence detect: [:irNode | irNode isTempVector and: [irNode name = aSymbol]].