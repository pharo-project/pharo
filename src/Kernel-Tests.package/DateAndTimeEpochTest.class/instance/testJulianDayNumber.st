tests
testJulianDayNumber
	self assert: aDateAndTime =  (DateAndTime julianDayNumber: 2415386).
	self assert: aDateAndTime julianDayNumber = 2415386.