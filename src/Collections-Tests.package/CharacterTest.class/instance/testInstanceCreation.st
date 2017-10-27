tests - instance creation
testInstanceCreation
	self should: [ Character value: -1 ] raise: Error.
	Character value: 0.
	Character value: 256