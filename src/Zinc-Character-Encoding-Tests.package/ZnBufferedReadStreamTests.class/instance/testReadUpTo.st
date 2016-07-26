testing
testReadUpTo
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 8.
	self assert: (stream upTo: $5) equals: '01234'.
	self assert: stream upToEnd equals: '6789'.
	self assert: stream atEnd