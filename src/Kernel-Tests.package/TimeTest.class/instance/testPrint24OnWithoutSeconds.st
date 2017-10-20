tests - printing
testPrint24OnWithoutSeconds
	self assert: (String streamContents: [ :str | aTime print24: true showSeconds: true on: str ]) equals: '12:34:56'