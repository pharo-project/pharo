Extension { #name : 'StringMorph' }

{ #category : '*Morphic-Widgets-Tree' }
StringMorph >> rowMorphForNode: aNode inColumn: aColumn [
	| hasIcon |
	hasIcon := (aColumn container iconBlock value: aNode) isNotNil.
	(aColumn isFirstColumn and: [hasIcon])
		ifTrue: [^ super rowMorphForNode: aNode inColumn: aColumn].
	self layoutInset: aColumn container columnInset @ aColumn container rowInset.
	self fitContents.
	^ self
]
