as yet unclassified
root: aRootObject extentBlock: extBlock childsBlock: childsBlock

	nodeExtentBlock := extBlock.
	nodeChildsBlock := childsBlock.

	root := self buildSubtreeFor: aRootObject level: 1.
	
	self layOutGeometry