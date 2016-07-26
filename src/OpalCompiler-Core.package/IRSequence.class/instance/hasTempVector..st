testing
hasTempVector: aSymbol
	^sequence anySatisfy: [:irNode | irNode isTempVector and: [irNode name = aSymbol]].