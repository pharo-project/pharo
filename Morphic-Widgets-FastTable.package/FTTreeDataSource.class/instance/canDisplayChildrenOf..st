testing
canDisplayChildrenOf: item
	^ self maxDepth ifNil: [ true ] ifNotNil: [ self maxDepth > item depth ]