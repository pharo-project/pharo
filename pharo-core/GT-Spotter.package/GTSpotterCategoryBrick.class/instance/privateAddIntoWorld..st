event handling
privateAddIntoWorld: aBrick
	super privateAddIntoWorld: aBrick.
	self setAmountVisible: self subbricksCount all: '...'