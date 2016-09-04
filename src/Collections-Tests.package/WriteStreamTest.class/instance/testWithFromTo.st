tests
testWithFromTo
	"like with: except it paritions its argument collection first using the
	from:/to: indexes"
	| stream |
	stream := self streamClass
		with: self string , self string
		from: 1
		to: self stringSize.
	self
		assert: stream originalContents = self string;
		assert: stream position = self stringSize;
		assert: stream contents = self string;
		assert: stream size = self stringSize.