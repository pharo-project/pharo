tests
testWriteStream
	| stream |
	stream := handle writeStream.
	self assert: (stream respondsTo: #nextPut:)