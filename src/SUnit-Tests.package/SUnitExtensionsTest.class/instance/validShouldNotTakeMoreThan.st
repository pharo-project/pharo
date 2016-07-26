real tests
validShouldNotTakeMoreThan

	self should: [(Delay forMilliseconds: 100) wait] notTakeMoreThan:  200 milliSeconds.