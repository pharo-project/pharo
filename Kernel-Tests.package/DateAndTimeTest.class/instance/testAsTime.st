tests
testAsTime
	| dateAndTime |
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 0 asDuration.
	self assert: dateAndTime asTime equals: (Time hour: 1 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 5 hours.
	self assert: dateAndTime asTime equals: (Time hour: 1 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: -5 hours.
	self assert: dateAndTime asTime equals: (Time hour: 1 minute: 2 second: 3).