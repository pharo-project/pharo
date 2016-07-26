tests - as string comma delimiter sequenceable
testAsStringOnDelimiterLastEmpty
	self assert: (String streamContents: [ :emptyStream | self empty asStringOn: emptyStream delimiter: ', ' last: 'and' ]) equals: ''