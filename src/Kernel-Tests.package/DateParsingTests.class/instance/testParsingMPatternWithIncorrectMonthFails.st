tests - m pattern
testParsingMPatternWithIncorrectMonthFails 
	self assertReading: '1.0.2013' as: 'dd.m.yyyy' raise: DateError.
	self assertReading: '22.13.2013' as: 'dd.m.yyyy' raise: DateError.