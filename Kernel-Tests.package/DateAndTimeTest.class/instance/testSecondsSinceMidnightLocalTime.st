tests
testSecondsSinceMidnightLocalTime
	| dateAndTime |
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 0 second: 56 offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 1 second: 56 offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 60+56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 0 second: 56 offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 3600+56.
	
	"1 hour offset"
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 0 second: 56 offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 1 second: 56 offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 60+56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 0 second: 56 offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 3600+56.
	
	"-1 hour offset"
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 0 second: 56 offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 56.
	
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 0 minute: 1 second: 56 offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 60+56.
	
	dateAndTime := DateAndTime year: 1002 day: 102 hour: 1 minute: 0 second: 56 offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 3600+56.