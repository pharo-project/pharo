scrollpane-scrolling
scrollDown: anInteger
	
	self band translateY: anInteger negated.
	self isBottomOverscrolled ifTrue: [ self moveBandBottom ]