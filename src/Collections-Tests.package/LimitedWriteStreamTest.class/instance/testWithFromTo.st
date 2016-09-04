tests
testWithFromTo
	"like with: except it paritions its argument collection first using the
	from:/to: indexes"
	| stream overLimit |
	
	super testWithFromTo.

	stream := self streamClass
		with: self string , self string
		from: 1
		to: self stringSize.
	overLimit := false.
	stream
		limit: self stringSize * 2;
		limitBlock: [ overLimit := true ].
	self string
		withIndexDo: [ :each :i | 
			self
				assert: (stream nextPut: each) = each;
				assert: stream position = (self stringSize + i);
				assert: stream contents = (self string , (self string first: i));
				deny: overLimit ].
	self string
		do: [ :each | 
			overLimit := false.
			self
				assert: (stream nextPut: each) = each;
				assert: stream position = (self stringSize * 2);
				assert: stream contents = (self string , self string);
				assert: overLimit ]