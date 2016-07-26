instance creation
breadthFirst: aReference collect: aBlock
	^ (self collect: aBlock) breadthFirst: aReference