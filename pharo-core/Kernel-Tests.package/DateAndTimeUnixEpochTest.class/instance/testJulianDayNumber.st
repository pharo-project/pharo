tests
testJulianDayNumber
	self assert: aDateAndTime =  (DateAndTime julianDayNumber: 2440588).
	self assert: aDateAndTime julianDayNumber = 2440588.