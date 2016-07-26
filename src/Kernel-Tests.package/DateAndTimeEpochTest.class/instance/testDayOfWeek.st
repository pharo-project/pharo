tests
testDayOfWeek
	self assert: aDateAndTime dayOfWeek  = 3.
	self assert: aDateAndTime dayOfWeekAbbreviation = 'Tue'.
	self assert: aDateAndTime dayOfWeekName = 'Tuesday'.
