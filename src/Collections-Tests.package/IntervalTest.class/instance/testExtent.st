tests
testExtent
	self assert: (1 to: 10) extent = 9.
	self assert: (1 to: 10 by: 2) extent = 9.
	self assert: (1 to:-1) extent = -2