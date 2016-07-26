tests - mathematical functions
testFloorLog
	self assert: (1/100 floorLog: 10) = -2.
	self assert: (((2 raisedTo: Float emax + 11)/3) floorLog: 10)
		= ((Float emax + 11)*2 log - 3 log) floor description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) floorLog: 10)
		= ((Float emin - Float precision)*2 log + 3 log) floor description: 'Fraction>>log should not underflow'