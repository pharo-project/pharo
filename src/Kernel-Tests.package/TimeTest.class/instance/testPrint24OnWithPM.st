tests - printing
testPrint24OnWithPM
	self assert: (String streamContents: [ :str | aTime print24: false on: str ]) equals: '12:34:56 pm'