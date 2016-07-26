tests - bogus date
testErrorWhenDayIsBeforeMonthStart
	self should: [ DateAndTime year: 2004 month: 2 day: -1 ] raise: Error.
	self should: [ DateAndTime year: 2004 month: 2 day: 0 ] raise: Error.
	DateAndTime year: 2004 month: 2 day: 1