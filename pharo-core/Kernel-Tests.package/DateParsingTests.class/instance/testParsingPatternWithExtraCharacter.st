tests
testParsingPatternWithExtraCharacter

	self assertReading: 'a4.2.2345' as: 'ad.m.y' equals: (Date year: 2345 month: 2 day: 4). 
	self assertReading: '4.2.2345' as: 'ad.m.y' raise: DateError.
	self assertReading: 'a4.2.2345' as: 'd.m.y' raise: DateError.