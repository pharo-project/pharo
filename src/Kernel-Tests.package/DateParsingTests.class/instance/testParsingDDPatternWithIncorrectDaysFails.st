tests - dd pattern
testParsingDDPatternWithIncorrectDaysFails 
	self assertReading: '02.00.2013' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.13.2013' as: 'dd.mm.yyyy' raise: DateError.
	