tests - mathematical functions
testLn
	self assert: (100 ln closeTo: 10 ln*2).
	self assert: ((2 raisedTo: Float emax + 3) ln closeTo: 2 ln*(Float emax + 3)) description: 'Integer>>ln should not overflow'