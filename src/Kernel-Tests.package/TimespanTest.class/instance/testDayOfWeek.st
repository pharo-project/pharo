tests
testDayOfWeek
	self assert: aTimespan  dayOfWeek  = 7.
	self assert: aTimespan  dayOfWeekName = 'Saturday'.
