tests
testWithComplexRecursion

	self should: [ self complexRecursion ] notTakeMoreThanMilliseconds: 3.