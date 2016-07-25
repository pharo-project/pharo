tests
testPlus
	self assert: aDateAndTime + '0:00:00:00' = aDateAndTime.
	self assert: aDateAndTime + 0 = aDateAndTime.
	self assert: aDateAndTime + aDuration = (DateAndTime year: 1970 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours )
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"
	
