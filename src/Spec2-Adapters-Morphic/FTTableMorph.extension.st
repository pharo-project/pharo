Extension { #name : #FTTableMorph }

{ #category : #'*Spec2-Adapters-Morphic' }
FTTableMorph >> clickOnColumnHeaderAt: anIndex [
	| header |
	header := self container headerRow submorphs at: anIndex.
	header click: (MouseButtonEvent basicNew 
							setType: #mouseDown;
							yourself).
		
]

{ #category : #'*Spec2-Adapters-Morphic' }
FTTableMorph >> hasFilter [

	function ifNil: [ ^ false ].
	^ function isKindOf: FTFilterFunction
]
