tests
testUnionWithSeparate

	self 
		assert: (anOverlappingTimespan union: aDisjointTimespan) = 
			(Timespan 
				starting: anOverlappingTimespan start
				ending:  (aDisjointTimespan end + DateAndTime clockPrecision))
			
