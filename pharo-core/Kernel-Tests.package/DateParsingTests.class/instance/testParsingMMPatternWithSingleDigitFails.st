tests - mm pattern
testParsingMMPatternWithSingleDigitFails
	self assertReading: '04.2.2345' as: 'dd.mm.yyyy' raise: DateError