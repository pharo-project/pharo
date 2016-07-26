instance creation
newBand
	^ super newBand height: [ :bandBrick |
		bandBrick subbricks isEmpty
			ifTrue: [ 0 ]
			ifFalse: [ bandBrick lastSubbrick brickBounds bottom ] ].