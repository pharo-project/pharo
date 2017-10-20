tests - printing
testPrintOn
	self assert: (String streamContents: [ :str | aTime printOn: str ]) equals: '12:34:56 pm'