layouting
layout: aBrick on: aTraverser

	"we need to calculate sizes of all subbricks"
	aTraverser layoutChildrenOf: aBrick.
	"first we calculate corresponding dimension"
	aTraverser updateDimension: aBrick.
	"after everything is done we can mark dimension as clean"
	aTraverser markClean: aBrick.
	"because my dimension changed, maybe my owner would like to adapt"
	aTraverser layoutOwnerOf: aBrick