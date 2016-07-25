tests
testRangeIncludes
	self
		assert: ((1 to: 10)
				rangeIncludes: 3).
	self
		assert: ((1 to: 10 by: 2)
				rangeIncludes: 3).
	self
		deny: ((1 to: 10)
				rangeIncludes: 0).
	self
		deny: ((1 to: 10)
				rangeIncludes: 11).
	self
		deny: ((1 to: 10 by: 2)
				rangeIncludes: 0).
	self
		deny: ((1 to: 10 by: 2)
				rangeIncludes: 11)