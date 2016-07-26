tests
testParsingPatternWithTrailingCharacterFails

	self assertReading: '04.02.2013trailing' as: 'd.m.y' raise: DateError