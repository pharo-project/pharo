tests
testPosition
	| stream |
	stream := self newStream.
	stream nextPutAll: self string.
	0 to: self stringSize do: [ :i | 
		self
			assert: stream position = self stringSize;
			assert: (stream position: i) == stream;
			assert: stream position = i;
			assert: stream contents = (self string first: i).
		stream position: self stringSize ].
	self should: [ stream position: -1 ] raise: Error.
	self should: [ stream position: self stringSize + 1 ] raise: Error