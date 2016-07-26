tests
testIntersectionWithSelf
	self assert: (aTimespan intersection: aTimespan)  = 
	(Timespan starting: jan01 duration: (Duration days: 6 hours: 23 minutes: 59 seconds: 59 nanoSeconds: 999999999)).		
	self deny: (aTimespan intersection: anIncludedTimespan)	= aTimespan
