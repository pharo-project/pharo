tests - as string comma delimiter sequenceable
testAsStringOnDelimiterEmpty
	self assert: (String streamContents: [ :emptyStream | self empty asStringOn: emptyStream delimiter: ', ' ]) equals: ''