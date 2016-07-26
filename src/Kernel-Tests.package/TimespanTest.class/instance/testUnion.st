tests
testUnion

	| union |
	union := timespan union: timespan.
	
	self 
		assert: (union start = timespan start);
		assert: (union duration = timespan duration)
