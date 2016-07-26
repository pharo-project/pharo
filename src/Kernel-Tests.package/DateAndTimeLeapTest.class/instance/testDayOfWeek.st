tests
testDayOfWeek
	self assert: aDateAndTime dayOfWeek  = 1.
	self assert: aDateAndTime dayOfWeekAbbreviation = 'Sun'.
	self assert: aDateAndTime dayOfWeekName = 'Sunday'.
