tests
testPrintOn
	self assert: (String streamContents: [ :str | aDuration printOn: str ]) equals: '1:02:03:04.000000005'