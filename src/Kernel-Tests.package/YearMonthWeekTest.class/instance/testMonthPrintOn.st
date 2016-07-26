tests
testMonthPrintOn
	| aMonth |
	aMonth := Month starting: DateAndTime new duration: 31 days.
	self assert: (String streamContents: [ :str | aMonth printOn: str ]) equals: 'January 1901'