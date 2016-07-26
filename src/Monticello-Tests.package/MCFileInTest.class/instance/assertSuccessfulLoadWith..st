testing
assertSuccessfulLoadWith: aBlock
	stream reset.
	aBlock value.
	self assertNoChange.
	self assertInitializersCalled.
	self assertInitializersOrder