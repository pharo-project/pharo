tests - mathematical functions
testLog
	self assert: ((1/100) log closeTo: -2).
	self assert: (((2 raisedTo: Float emax + 11)/3) log closeTo: (Float emax + 11)*2 log - 3 log) description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) log closeTo: (Float emin - Float precision)*2 log + 3 log) description: 'Fraction>>log should not underflow'