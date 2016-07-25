tests
testOffset
	self assert: aDateAndTime offset =  '0:00:00:00' asDuration.
     self assert: (aDateAndTime offset: '0:12:00:00') equals:  '1970-01-01T12:00:00+12:00' asDateAndTime