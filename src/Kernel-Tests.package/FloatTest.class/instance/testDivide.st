tests - arithmetic
testDivide

	self assert: 1.5 / 2.0 = 0.75.
	
	self assert: 2.0 / 1 = 2.0.
	
	self should: [ 2.0 / 0 ] raise: ZeroDivide.
	self should: [ 2.0 / 0.0 ] raise: ZeroDivide.
	self should: [ 1.2 / Float negativeZero ] raise: ZeroDivide.
	self should: [ 1.2 / (1.3 - 1.3) ] raise: ZeroDivide
	