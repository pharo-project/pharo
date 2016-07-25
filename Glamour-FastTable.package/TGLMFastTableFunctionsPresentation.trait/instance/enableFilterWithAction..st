public api
enableFilterWithAction: aBlock
	self enableFilter.
	self parameters at: 'actionBlock' put: aBlock