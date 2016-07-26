tests
testJulianDayNumber
	self assert: aDateAndTime =  (DateAndTime julianDayNumber: 2444240).
	self assert: aDateAndTime julianDayNumber = 2444240.