*Morphic-Base-MorphTreeWidget
rowMorphForNode: aNode inColumn: aColumn
	| hasIcon |
	hasIcon := (aColumn container iconBlock value: aNode) notNil.
	(aColumn isFirstColumn and: [hasIcon]) 
		ifTrue: [^ super rowMorphForNode: aNode inColumn: aColumn].
	self layoutInset: aColumn container columnInset @ aColumn container rowInset.
	self fitContents.
	^ self 