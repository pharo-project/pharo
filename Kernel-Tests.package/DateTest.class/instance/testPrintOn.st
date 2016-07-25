tests
testPrintOn
	self assert: (String streamContents: [ :str | january23rd2004 printOn: str ]) equals: '23 January 2004'