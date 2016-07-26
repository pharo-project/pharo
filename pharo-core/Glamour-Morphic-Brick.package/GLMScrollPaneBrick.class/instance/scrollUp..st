scrollpane-scrolling
scrollUp: anInteger

	self band translateY: anInteger.
	self isTopOverscrolled ifTrue: [ self moveBandTop ]