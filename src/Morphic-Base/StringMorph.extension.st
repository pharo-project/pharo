Extension { #name : #StringMorph }

{ #category : #'*Morphic-Base-Widgets' }
StringMorph >> heightToDisplayInList: aList [

	^ self contents heightToDisplayInList: aList 
]

{ #category : #'*Morphic-Base-MorphTreeWidget' }
StringMorph >> rowMorphForNode: aNode inColumn: aColumn [
	| hasIcon |
	hasIcon := (aColumn container iconBlock value: aNode) notNil.
	(aColumn isFirstColumn and: [hasIcon]) 
		ifTrue: [^ super rowMorphForNode: aNode inColumn: aColumn].
	self layoutInset: aColumn container columnInset @ aColumn container rowInset.
	self fitContents.
	^ self 
]

{ #category : #'*Morphic-Base-Widgets' }
StringMorph >> widthToDisplayInList: aList [

	^ self contents widthToDisplayInList: aList 
]
