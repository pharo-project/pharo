tests - conversion
testFloor
	self assert: 1.0 floor = 1.
	self assert: 1.1 floor = 1.
	self assert: -2.0 floor = -2.
	self assert: -2.1 floor = -3.