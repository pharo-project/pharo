testing
testReadUpToEnd
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 4.
	stream next: 2.
	self assert: stream upToEnd equals: '23456789'.
	self assert: stream atEnd