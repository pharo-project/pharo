tests
testAsTimeUTC
	| dateAndTime |
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 0 asDuration.
	self assert: dateAndTime asTimeUTC equals: (Time hour: 1 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: -5 hours.
	self assert: dateAndTime asTimeUTC equals: (Time hour: 6 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 5 hours.
	self assert: dateAndTime asTimeUTC equals: (Time hour: 24+1-5 minute: 2 second: 3).