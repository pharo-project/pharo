tests
testPrintOn
	self assert: (String streamContents: [ :str | aStopwatch printOn: str ]) equals: 'a Stopwatch(suspended:0:00:00:00)'