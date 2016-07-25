tests
testHour12
	self assert: aDateAndTime hour12  = DateAndTime new hour12.
	self assert: aDateAndTime hour12  = 12
