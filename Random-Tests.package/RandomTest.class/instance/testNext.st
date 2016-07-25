tests
testNext

	10000 timesRepeat: [ 
		| next | 
		next := gen next.
		self assert: (next >= 0).
		self assert: (next < 1).
	].