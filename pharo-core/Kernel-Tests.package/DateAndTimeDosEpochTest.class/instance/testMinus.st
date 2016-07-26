tests
testMinus
	self assert: aDateAndTime - aDateAndTime =  '0:00:00:00' asDuration.
	self assert: aDateAndTime - '0:00:00:00' asDuration = aDateAndTime.
	self assert: aDateAndTime - aDuration =  (DateAndTime year: 1979 month: 12 day: 30 hour: 21 minute: 56 second: 55 nanoSecond: 999999995 offset: 0 hours ).
	" I believe this Failure is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)" 