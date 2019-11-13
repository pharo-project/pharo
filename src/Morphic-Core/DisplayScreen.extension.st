Extension { #name : #DisplayScreen }

{ #category : #'*Morphic-Core' }
DisplayScreen >> boundingBox [

	ActiveWorld ifNil: [ ^ 0@0 corner: 0@0 ].
	^ ActiveWorld bounds
]
