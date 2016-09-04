tests
testSetToEnd
	| stream |
	stream := self newStream.
	self
		assert: stream setToEnd == stream;
		assert: stream position isZero;
		assert: stream contents isEmpty.
	stream nextPutAll: self string.
	self stringSize - 1 to: 0 by: -1 do: [ :i | 
		stream position: i.
		self
			assert: stream setToEnd == stream;
			assert: stream position = self stringSize;
			assert: stream contents = self string ]