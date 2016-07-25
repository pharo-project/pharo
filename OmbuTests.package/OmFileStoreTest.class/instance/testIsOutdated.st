tests
testIsOutdated

	| outdatedStore |
	store newEntry: (OmEntry content: 42).
	
	outdatedStore := store copyReopened.
	
	store newEntry: (OmEntry content: 43).

	self deny: store isOutdated.
	self assert: outdatedStore isOutdated.