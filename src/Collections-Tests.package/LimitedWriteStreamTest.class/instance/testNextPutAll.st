tests
testNextPutAll
	| stream overLimit |
	super testNextPutAll.
	stream := self streamClass
		on: String new
		limit: self stringSize
		limitBlock: [ overLimit := true ].
	overLimit := false.
	self
		assert: (stream nextPutAll: self string) = self string;
		assert: stream position = self stringSize;
		assert: stream contents = self string;
		deny: overLimit.
	self
		assert: (stream nextPutAll: self string) = self string;
		assert: stream position = self stringSize;
		assert: stream contents = self string;
		assert: overLimit