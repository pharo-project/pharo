tests
testYearMonthDayHourMinuteSecondNanosSecondOffset
	self assert: aDateAndTime =  (DateAndTime year: 1901 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset:0 hours ).
	self assert: ((DateAndTime year: 1 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset: 0 hours ) +
				(Duration days: 1 hours: 2 minutes: 3 seconds: 4  nanoSeconds: 5) ) =  	
				(DateAndTime year: 1 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours ) 
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"   
