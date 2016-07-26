tests
testError
	self should: [ self runSimulated: [self methodWithError] ] raise: Error