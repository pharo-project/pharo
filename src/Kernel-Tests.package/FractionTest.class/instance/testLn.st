tests - mathematical functions
testLn
	self assert: ((1/100) ln closeTo: -2 * 10 ln).
	self assert: (((2 raisedTo: Float emax + 11)/3) ln closeTo: (Float emax + 11)*2 ln - 3 ln) description: 'Fraction>>ln should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) ln closeTo: (Float emin - Float precision)*2 ln + 3 ln) description: 'Fraction>>ln should not underflow'