tests - d pattern
testParsingDPatternWithTwoDigitSucceeds 
	self assertReading: '29.11.2013' as: 'd.mm.yyyy' equals: (Date year: 2013 month: 11 day: 29).