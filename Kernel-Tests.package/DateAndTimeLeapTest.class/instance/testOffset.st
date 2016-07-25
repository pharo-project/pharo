tests
testOffset

	self assert: aDateAndTime offset =  '0:02:00:00' asDuration.
	self assert: (aDateAndTime offset: '-0:12:00:00') equals: '2004-02-28T23:33:00-12:00' asDateAndTime.
	self assert: (aDateAndTime offset: '0:12:00:00') equals:  '2004-02-29T23:33:00+12:00' asDateAndTime