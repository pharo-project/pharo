tests
testIntersectionWithOverlapping
	self assert: (aTimespan intersection: anOverlappingTimespan)  = 
	(Timespan starting: jan01 duration: (Duration days: 5 hours: 23 minutes: 59 seconds: 59 nanoSeconds: 999999999)).		

