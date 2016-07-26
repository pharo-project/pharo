tests
testPrintOn
	self assert: (String streamContents: [ :str | aTimespan printOn: str ]) equals: 'a Timespan(2005-01-01T00:00:00+00:00D7:00:00:00)'