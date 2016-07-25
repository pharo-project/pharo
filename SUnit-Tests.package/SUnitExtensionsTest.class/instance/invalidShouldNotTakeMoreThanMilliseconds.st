real tests
invalidShouldNotTakeMoreThanMilliseconds

	self should: [(Delay forMilliseconds: 100) wait] notTakeMoreThanMilliseconds: 50