*Glamour-Morphic-Brick
brickThemer
	^ (self nearestOwnerThat: [ :each | (each isKindOf: GLMFoundationBrick) | (each isKindOf: GLMBrick) ])
		ifNotNil: [ :brick | brick themer ]
		ifNil: [ self theme brickThemer ]