testing
testException

	self
		should: [self error: 'foo']
		raise: self defaultTestError