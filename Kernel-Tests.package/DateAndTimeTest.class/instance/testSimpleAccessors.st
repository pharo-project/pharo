tests - instance
testSimpleAccessors

	| t |
	t := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	self 
		assert: (t hours = t hours);
		assert: (t minutes = t minute);
		assert: (t seconds = t second).
