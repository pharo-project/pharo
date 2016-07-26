action
toggleExpand
	expanded := expanded not.
	self initializeSelectors.
	self narrowWith: narrowString