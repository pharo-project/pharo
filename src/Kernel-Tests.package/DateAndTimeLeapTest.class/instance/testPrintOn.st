tests
testPrintOn
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '2004-02-29T13:33:00+02:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(UTC)'