testing
hasConfiguration
	self flag: 'TODO: should use proper cli parser Coral??'.
	^ (self arguments size > 1) and: [
		((self argumentAt: 2) beginsWith: '-') not ]