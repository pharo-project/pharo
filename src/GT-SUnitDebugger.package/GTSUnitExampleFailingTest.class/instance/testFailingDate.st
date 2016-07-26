tests
testFailingDate
	|aDateAndTime|

	aDateAndTime :=  DateAndTime localTimeZone: TimeZone default; dosEpoch.
	self 
		assert: (aDateAndTime offset: '0:12:00:00')  
		equals: '1 January 1980 1:00 pm' asDateAndTime