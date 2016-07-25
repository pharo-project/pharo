tests - instance creation
testNew
	self should: [Integer new] raise: self defaultTestError. 