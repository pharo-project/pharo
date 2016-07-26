tests - bogus date
testErrorWhenDayIsAfterMonthEnd
	self should: [ DateAndTime year: 2004 month: 2 day: 30 ] raise: Error.
	DateAndTime year: 2004 month: 2 day: 29