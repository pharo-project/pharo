tests
testDefaultParsingSucceeds 
	| date |
	date := Date year: 2013 month: 11 day: 29.
	self assertReading: '29.11.2013' as: 'dd.mm.yyyy' equals: date.
	self assertReading: '29.11.2013' as: 'd.m.y' equals: date.
	self assertReading: '29.11.13' as: 'd.m.y' equals: date