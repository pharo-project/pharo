tests - dd pattern
testParsingDDPatternWithSingleDigitFails
	self assertReading: '4.02.2345' as: 'dd.mm.yyyy' raise: DateError