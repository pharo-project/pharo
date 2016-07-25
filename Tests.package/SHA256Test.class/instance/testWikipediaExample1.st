testing
testWikipediaExample1
	hash := SHA256 hashMessage: 'The quick brown fox jumps over the lazy dog'.
	self assert: hash hex equals: 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592'