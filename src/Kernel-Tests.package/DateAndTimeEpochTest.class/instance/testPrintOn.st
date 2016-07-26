tests
testPrintOn
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '1901-01-01T00:00:00+00:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(ETZ)'