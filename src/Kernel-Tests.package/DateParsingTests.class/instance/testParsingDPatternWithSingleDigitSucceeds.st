tests - d pattern
testParsingDPatternWithSingleDigitSucceeds
	self assertReading: '2.11.2013' as: 'd.mm.yyyy' equals: (Date year: 2013 month: 11 day: 2)