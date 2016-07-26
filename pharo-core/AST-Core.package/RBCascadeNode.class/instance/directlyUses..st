testing
directlyUses: aNode 
	^messages last = aNode and: [self isDirectlyUsed]