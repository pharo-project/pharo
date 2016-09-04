tests
testLimit
	| stream overLimit |
	stream := self newStream.
	overLimit := false.
	stream
		limitBlock: [ overLimit := true ];
		nextPutAll: self string.
	self
		assert: stream limit = self streamClass defaultLimit;
		assert: (stream limit: self stringSize) == stream;
		assert: stream limit = self stringSize;
		deny: overLimit.

	self stringSize - 1 to: 0 by: -1 do: [:i |
		overLimit := false.
		self
			assert: (stream limit: i) == stream;
			assert: stream limit = i;
			assert: stream position = i;
			assert: stream contents = (self string first: i);
			assert: overLimit].