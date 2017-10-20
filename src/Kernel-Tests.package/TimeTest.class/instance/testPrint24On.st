tests - printing
testPrint24On
	self assert: (String streamContents: [ :str | aTime print24: true on: str ]) equals: '12:34:56'