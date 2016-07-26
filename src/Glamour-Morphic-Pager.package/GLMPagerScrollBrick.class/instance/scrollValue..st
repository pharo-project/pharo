scrolling
scrollValue: value
	
	currentScrollDelta := value.
	
	self band doNotLayoutDuring: [ :b | b translateXTo: self scrollValue ].
	self changed