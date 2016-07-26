tests
testByWeekNumber
	"Check some week dates, assuming that the week starts on Sunday"
	week := Week year: 2013 week: 1.
	self assert: week start equals: (DateAndTime year: 2012 month: 12 day: 30).
	
	week := Week year: 2013 week: 32.
	self assert: week start equals: (DateAndTime year: 2013 month: 8 day: 4).
	
	week := Week year: 2013 week: 52.
	self assert: week start equals: (DateAndTime year: 2013 month: 12 day: 22).
	
	week := Week year: 2014 week: 1.
	self assert: week start equals: (DateAndTime year: 2013 month: 12 day: 29).