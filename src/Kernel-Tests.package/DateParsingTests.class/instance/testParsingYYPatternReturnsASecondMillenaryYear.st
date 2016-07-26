tests - yy pattern
testParsingYYPatternReturnsASecondMillenaryYear
	self assertReading: '2.11.13' as: 'd.mm.yy' equals: (Date year: 2013 month: 11 day: 2)