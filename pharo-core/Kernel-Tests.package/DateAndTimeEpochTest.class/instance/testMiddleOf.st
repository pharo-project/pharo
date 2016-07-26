tests
testMiddleOf
	self assert: (aDateAndTime middleOf: '2:00:00:00' asDuration) = 
	 (Timespan starting: '12-31-1900' asDate duration: 2 days).
	
