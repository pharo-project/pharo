layouting
layout: aBrick on: aTraverser

	aBrick owner ifNil: [ ^ aTraverser markClean: aBrick ].
	(aTraverser isDependsOnChildren: aBrick owner) ifTrue: [ Error signal: 'Circle dependency detected!' ].
	
	"because I depend on my parent, it should be layouted first"
	aTraverser layoutOwnerOf: aBrick.
	"then I can update my dimension"
	aTraverser updateDimension: aBrick.
	"everething is done I can mark dimension as clean"
	aTraverser markClean: aBrick.
	"and I can say my children to layout themselves, because I'm already clean"
	aTraverser layoutChildrenOf: aBrick