tests - arithmetic
testDivide

	self assert: 2 / 1 = 2.
	self assert: (3 / 2) isFraction.
	self assert: 4 / 2 = 2.
	self should: [ 1 / 0 ] raise: ZeroDivide.