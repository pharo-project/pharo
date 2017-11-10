Extension { #name : #TransformationMorph }

{ #category : #'*Athens-Morphic' }
TransformationMorph >> drawOnAthensCanvas: anAthensCanvas [
	submorphs isEmpty ifTrue: [super drawOnAthensCanvas: anAthensCanvas]
]
