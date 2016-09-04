tests
testNextPutAll
	| stream |
	stream := self newStream.
	self
		assert: (stream nextPutAll: self string) = self string;
		assert: stream position = self stringSize;
		assert: stream contents = self string