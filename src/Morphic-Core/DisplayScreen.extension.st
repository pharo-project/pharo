Extension { #name : #DisplayScreen }

{ #category : #'*Morphic-Core' }
DisplayScreen >> boundingBox [

	self currentWorld ifNil: [ ^ 0@0 corner: 0@0 ].
	^ self currentWorld bounds
]
