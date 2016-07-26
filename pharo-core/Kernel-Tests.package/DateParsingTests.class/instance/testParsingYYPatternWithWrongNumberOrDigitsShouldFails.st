tests - yy pattern
testParsingYYPatternWithWrongNumberOrDigitsShouldFails
	self assertReading: '02.11.3' as: 'dd.mm.yy' raise: DateError.
	self assertReading: '02.11.113' as: 'dd.mm.yy' raise: DateError