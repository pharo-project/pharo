tests
testDuration
	self assert: aTimespan duration  = aWeek.
	aTimespan duration: aDay.
	self assert: aTimespan duration =  aDay.

