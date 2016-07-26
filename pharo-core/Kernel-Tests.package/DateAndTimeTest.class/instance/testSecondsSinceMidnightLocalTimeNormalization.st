tests
testSecondsSinceMidnightLocalTimeNormalization
	| dateAndTime|
	
	"Check offset: changes from times defined in UTC"
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 0 minute: 1 second: 56 offset: 0 hours.
	dateAndTime := dateAndTime offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (1*3600) + (1*60) + 56.
	"-1 hours will switch to the previous day"
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (23*3600) + (1*60) + 56.
	
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 23 minute: 1 second: 56 offset: 0 hours.
	dateAndTime := dateAndTime offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (0*3600) + (1*60) + 56.
	"-1 hours will switch to the next day"
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 22*3600 + (1*60) + 56.	
		
		
	
	"Check offset: changes from times defined in +1"
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 0 minute: 1 second: 56 offset: 1 hours.
	dateAndTime := dateAndTime offset: 2 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (1*3600) + (1*60) + 56.
	"0 hours will switch to the previous day"
	dateAndTime := dateAndTime offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (23*3600) + (1*60) + 56.
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (22*3600) + (1*60) + 56.
	
	
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 23 minute: 1 second: 56 offset: 1 hours.
	"+2 hours will switch to the next day"
	dateAndTime := dateAndTime offset: 2 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (0*3600) + (1*60) + 56.
	dateAndTime := dateAndTime offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (22*3600) + (1*60) + 56.
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (21*3600) + (1*60) + 56.
