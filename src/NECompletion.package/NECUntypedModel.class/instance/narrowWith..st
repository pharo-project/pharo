action
narrowWith: aString 
	self narrowString: aString.
	self resetEntries.
	self addVariables.
	self addSelectors