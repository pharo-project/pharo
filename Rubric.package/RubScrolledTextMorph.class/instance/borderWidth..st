accessing scrollbars
borderWidth: anInteger
	super borderWidth: anInteger.
	self extent: self extent.
	self manageLayout 