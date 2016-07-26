tests
testHour
	self assert: aDateAndTime hour =    aDateAndTime hour24.
	self assert: aDateAndTime hour =    0.
	self assert: aDateAndTime hour =    aDateAndTime hours
