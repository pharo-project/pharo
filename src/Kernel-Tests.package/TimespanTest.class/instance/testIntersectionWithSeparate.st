tests
testIntersectionWithSeparate
	self assert: (aTimespan intersection: aDisjointTimespan) isNil.
	self deny: (aTimespan intersection: anOverlappingTimespan) isNil.
	self assert: (aTimespan intersection: anIncludedTimespan)  = 
	(Timespan starting: jan01 duration: (Duration days: 0 hours: 23 minutes: 59 seconds: 59 nanoSeconds: 999999999)).		
	self deny: (aTimespan intersection: anIncludedTimespan)	= anIncludedTimespan
