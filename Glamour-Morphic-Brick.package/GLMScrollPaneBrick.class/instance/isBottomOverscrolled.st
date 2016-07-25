scrollpane-testing
isBottomOverscrolled

	self isLastVisible ifFalse: [ ^ false ].
	self band brickBounds bottom < self height ifTrue: [ ^ true ].
	^ ((self band lastSubbrick brickBounds bottom + self band brickBounds top) < self height)