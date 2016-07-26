initialization
initializeOwner: aMenuItem keyText: aString
	self initialize.
	owner := aMenuItem.
	keyText := self class normalize: aString