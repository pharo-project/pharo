tests
testOffset
	| dateAndTime1 dateAndTime2 |
	
	dateAndTime1 :=  DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 1 hours.
	dateAndTime2 := dateAndTime1 offset: 1 hour.
	self assert: dateAndTime1 equals: dateAndTime2.
	self assert: dateAndTime1 localSeconds equals: dateAndTime2 localSeconds.
	
	dateAndTime2 := dateAndTime1 offset: -1 hour.
	self assert: dateAndTime1 equals: dateAndTime2.
	self deny: dateAndTime1 localSeconds == dateAndTime2 localSeconds.
	
	dateAndTime2 := dateAndTime1 offset: -2 hour.
	self assert: dateAndTime1 equals: dateAndTime2.
	self deny: dateAndTime1 localSeconds == dateAndTime2 localSeconds.