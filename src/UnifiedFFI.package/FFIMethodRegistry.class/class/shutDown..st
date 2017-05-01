system startup
shutDown: quitting
	quitting ifFalse: [ ^ self ].
	
	self resetAll