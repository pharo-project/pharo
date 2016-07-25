real tests
invalidShouldNotTakeMoreThan

	self should: [(Delay forMilliseconds: 100) wait] notTakeMoreThan: 50 milliSeconds.