top window
activateAndForceLabelToShow
	self activate.
	bounds top < 0 ifTrue:
		[self position: (self position x @ 0)]