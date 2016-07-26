tests
testYearPrintOn
	| aYear |
	aYear := Year starting: DateAndTime new duration: 365 days.
	self assert: (String streamContents: [ :str | aYear printOn: str ]) equals: 'a Year (1901)'