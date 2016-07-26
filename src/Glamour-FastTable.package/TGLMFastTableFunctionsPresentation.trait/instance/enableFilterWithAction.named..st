public api
enableFilterWithAction: aBlock named: aString
	self enableFilter.
	self parameters
		at: 'actionBlock' put: aBlock;
		at: 'actionName' put: aString