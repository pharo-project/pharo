tests - mathematical functions
testLog
	self assert: (100 log closeTo: 2).
	self assert: ((2 raisedTo: Float emax + 3) log closeTo: 2 log*(Float emax + 3)) description: 'Integer>>log should not overflow'