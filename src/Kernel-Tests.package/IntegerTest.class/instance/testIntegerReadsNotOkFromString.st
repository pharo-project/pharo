tests - instance creation
testIntegerReadsNotOkFromString
	self should: [Integer readFrom: 'aaa'] raise: Error.
	self should: [Integer readFrom: '-aaa'] raise: Error.
	self should: [Integer readFrom: 'a3' base: 8] raise: Error.