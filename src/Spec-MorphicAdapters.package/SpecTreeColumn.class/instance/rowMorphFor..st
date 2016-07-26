column drawing
rowMorphFor: aNode 
	| node rowMorph |
	
	node := aNode nodeModel.
	rowMorph := self rowMorphGetSelector
		ifNil: [node rowMorphForColumn: self]
		ifNotNil: [self rowMorphGetterBlock value: node value: self container].
	rowMorph := rowMorph asMorph.
	rowMorph
		ifNotNil: [rowMorph borderWidth: 0]
		ifNil: [rowMorph := self defaultRowMorph].
	^ rowMorph rowMorphForNode: node inColumn: self
