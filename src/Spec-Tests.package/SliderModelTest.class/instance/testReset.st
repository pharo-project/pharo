tests
testReset
	self initMinMax.
	testedInstance
		value: 50;
		reset.
	self assert: testedInstance value == 0