tests - instance
testInstanceCreation

	| t |
	t := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	self 
		assert: (t julianDayNumber = 1721427);
		assert: (t offset = 6 hours);
		assert: (t hour = 2);
		assert: (t minute = 3);
		assert: (t second = 4);
		assert: (t nanoSecond = 5).
		
