Extension { #name : #FTTableMorph }

{ #category : #'*Spec2-Adapters-Morphic' }
FTTableMorph >> hasFilter [

	function ifNil: [ ^ false ].
	^ function isKindOf: FTFilterFunction
]
