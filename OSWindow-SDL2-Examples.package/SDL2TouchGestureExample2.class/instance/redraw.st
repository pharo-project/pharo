drawing
redraw
	discoMode ifTrue: 
		[ self randomizeCircleColor ].
	self clearSurface.
	self drawBlackBackground.
	self drawMoves.
	self drawCircle.
	hasAbortedSwipe
		ifTrue: [ self displayAbortMessage ]