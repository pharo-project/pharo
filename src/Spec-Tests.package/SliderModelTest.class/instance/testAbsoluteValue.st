tests
testAbsoluteValue
	self initMinMax.
	testedInstance absoluteValue: 0.5.
	self assert: testedInstance value == 50