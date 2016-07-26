asserting
denyButtonOn: aString
	self deny: (self findButtonWithLabel: aString) getModelState.
	