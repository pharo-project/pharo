tests
testOnFromTo
	| stream |
	stream := self streamClass
		on: self string , self string
		from: self stringSize + 1
		to: self stringSize * 2.
	self
		assert: stream originalContents = (self string, self string);
		assert: stream position = self stringSize;
		assert: stream contents = self string;
		assert: stream size = (self stringSize * 2)