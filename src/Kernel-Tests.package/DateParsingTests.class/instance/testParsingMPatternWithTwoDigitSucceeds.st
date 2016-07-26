tests - m pattern
testParsingMPatternWithTwoDigitSucceeds 
	| date |
	date := Date year: 2013 month: 11 day: 29.
	self assertReading: '29.11.2013' as: 'dd.m.yyyy' equals: date.
	self assertReading: '11.2013.29' as: 'm.yyyy.dd' equals: date.
	self assertReading: '29.2013.11' as: 'dd.yyyy.m' equals: date.