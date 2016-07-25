initialization
initialize
	super initialize.
	self contract.
	table := NOCCompletionTable default.
	classTable := NOCCompletionTable classes.