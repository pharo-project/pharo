tests
testThreadSafe

	self should: [ self threadSafe ] notTakeMoreThanMilliseconds: 10.
	
	self assert: fork isNil.