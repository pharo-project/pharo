Extension { #name : #DummyUIManager }

{ #category : #'*Graphics-Display Objects' }
DummyUIManager >> newDisplayDepthNoRestore: pixelSize [

	"Change depths.  Check if there is enough space!  , di"
	| area need |
	pixelSize = Display depth ifTrue: [^ self  "no change"].
	pixelSize abs < Display depth ifFalse:
		["Make sure there is enough space"
		area := Display boundingBox area. "pixels"

		need := (area * (pixelSize abs - Display depth) // 8)  "new bytes needed"
				+ Smalltalk lowSpaceThreshold.
		(Smalltalk garbageCollectMost <= need
			and: [Smalltalk garbageCollect <= need])
			ifTrue: [self error: 'Insufficient free space']].
	Display setExtent: Display extent depth: pixelSize.

	DisplayScreen startUp
]
