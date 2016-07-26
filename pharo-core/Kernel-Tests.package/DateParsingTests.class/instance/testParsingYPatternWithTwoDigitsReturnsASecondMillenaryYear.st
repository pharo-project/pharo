tests - y pattern
testParsingYPatternWithTwoDigitsReturnsASecondMillenaryYear
	self assertReading: '2.11.13' as: 'd.mm.y' equals: (Date year: 2013 month: 11 day: 2)