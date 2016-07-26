tests - d pattern
testParsingDPatternWithIncorrectDayFails 
	self assertReading: '0.11.2013' as: 'd.mm.yyyy' raise: DateError.
	self assertReading: '32.2013.29' as: 'd.mm.yyyy' raise: DateError.