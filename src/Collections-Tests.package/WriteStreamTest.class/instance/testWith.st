tests
testWith
	"like on: except it starts writing at the end of its argument collection,
	and the initial position is the collection size and the initial contents
	is the collection"
	| stream |
	stream := self streamClass with: self string.
	self
		assert: stream originalContents = self string;
		assert: stream position = self stringSize;
		assert: stream contents = self string;
		assert: stream size = self stringSize.