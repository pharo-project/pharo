tests - instance creation
testIntegerReadsNotOkFromStream
	self should: [Integer readFrom: 'a23' readStream] raise: Error.
	self should: [Integer readFrom: '-a23' readStream] raise: Error.
	self should: [Integer readFrom: 'a3' readStream base: 8] raise: Error.
	self should: [Integer readFrom: '-a3' readStream base: 8] raise: Error.