tests - y pattern
testParsingYPatternWithFiveDigits
	self assertReading: '2.11.10000' as: 'd.mm.y' equals: (Date year: 10000 month: 11 day: 2)