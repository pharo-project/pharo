event handling
focusChanged 
	super focusChanged.
	self editor focusChanged.
	self changed.

	
