event handling
keyStroke: event fromMorph: morph

	keystrokeSelector ifNotNil: [ 
		^ self keystrokeAction: event ].
	
	^ false