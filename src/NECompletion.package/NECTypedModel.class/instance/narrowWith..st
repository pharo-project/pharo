action
narrowWith: aString 
	self
		narrowString: aString ;
		initializeSelectors.
	entries reset.
	self addToEntries: selectors