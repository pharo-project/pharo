tests - basic
testLargePrimesUpTo

	| nn | 
	nn := (2 raisedTo: 17) - 1. 
	self deny: (Integer primesUpTo: nn) last = nn.
	self assert: (Integer primesUpTo: nn + 1) last  = nn.
	
	
