tests
testWithRecursion

	self should: [ self recursion ] notTakeMoreThanMilliseconds: 3.