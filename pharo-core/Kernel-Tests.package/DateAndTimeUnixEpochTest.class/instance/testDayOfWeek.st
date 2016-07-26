tests
testDayOfWeek
	self assert: aDateAndTime dayOfWeek  = 5.
	self assert: aDateAndTime dayOfWeekAbbreviation = 'Thu'.
	self assert: aDateAndTime dayOfWeekName = 'Thursday'.
