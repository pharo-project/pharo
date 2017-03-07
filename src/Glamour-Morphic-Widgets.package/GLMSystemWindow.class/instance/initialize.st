initialization
initialize 
	super initialize.
	self
		on: #keyStroke 
		send: #handleKeyStroke: 
		to: self.
	hasInitialStep := false