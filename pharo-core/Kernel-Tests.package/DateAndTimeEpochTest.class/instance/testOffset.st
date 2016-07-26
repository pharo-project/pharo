tests
testOffset

	self assert: aDateAndTime offset =  '0:00:00:00' asDuration.
	self assert: (aDateAndTime offset: '-0:12:00:00') equals:  '1900-12-31T12:00:00-12:00' asDateAndTime.
	self assert: (aDateAndTime offset: '0:12:00:00') equals:  '1901-01-01T12:00:00+12:00' asDateAndTime