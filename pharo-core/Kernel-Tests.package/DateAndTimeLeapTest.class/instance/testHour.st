tests
testHour
	self assert: aDateAndTime hour equals: aDateAndTime hour24.
	self assert: aDateAndTime hour equals: 13.
	self assert: aDateAndTime hour equals: aDateAndTime hours
