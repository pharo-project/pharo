testing
directlyUses: aNode 
	^aNode = value ifTrue: [true] ifFalse: [self isDirectlyUsed]