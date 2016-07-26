tests - yyyy pattern
testParsingYYYYPatternWithWrongNumberOrDigitsShouldFails
	self assertReading: '02.11.3' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.11.13' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.11.113' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.11.10113' as: 'dd.mm.yyyy' raise: DateError