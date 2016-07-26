layouting
layout: aBrick on: aTraverser

	"immidiately set my dimension"
	aTraverser updateDimension: aBrick.
	aTraverser markClean: aBrick.
	aTraverser layoutChildrenOf: aBrick.
	aTraverser layoutOwnerOf: aBrick