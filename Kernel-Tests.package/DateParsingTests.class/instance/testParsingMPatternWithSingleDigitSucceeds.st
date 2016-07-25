tests - m pattern
testParsingMPatternWithSingleDigitSucceeds 
	| date |
	date := Date year: 2013 month: 2 day: 11.
	self assertReading: '11.2.2013' as: 'dd.m.yyyy' equals: date.
	self assertReading: '2.2013.11' as: 'm.yyyy.dd' equals: date.
	self assertReading: '11.2013.2' as: 'dd.yyyy.m' equals: date