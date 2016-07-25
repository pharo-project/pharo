initialize-release
initialize
	super initialize.
	styles := TxTextStyles on: self.
	self initializeContents.
	