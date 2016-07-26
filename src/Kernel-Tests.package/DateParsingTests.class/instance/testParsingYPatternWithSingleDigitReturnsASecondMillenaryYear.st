tests - y pattern
testParsingYPatternWithSingleDigitReturnsASecondMillenaryYear
	self assertReading: '2.11.3' as: 'd.mm.y' equals: (Date year: 2003 month: 11 day: 2)