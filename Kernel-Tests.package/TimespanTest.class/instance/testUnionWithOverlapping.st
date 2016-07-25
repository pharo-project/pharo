tests
testUnionWithOverlapping

	self 
		assert: (aTimespan union: anOverlappingTimespan)  = 
				(Timespan starting: dec31 duration: (8 days))