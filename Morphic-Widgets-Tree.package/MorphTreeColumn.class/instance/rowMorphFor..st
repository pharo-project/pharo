column drawing
rowMorphFor: aNode 
	| rowMorph |
	
	rowMorph := self rowMorphGetSelector
		ifNil: [aNode rowMorphForColumn: self]
		ifNotNil: [self rowMorphGetterBlock value: aNode value: self container].
	rowMorph := rowMorph asMorph.
	rowMorph
		ifNotNil: [rowMorph borderWidth: 0]
		ifNil: [rowMorph := self defaultRowMorph].
	^ rowMorph rowMorphForNode: aNode inColumn: self
