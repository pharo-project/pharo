tests
testNextPut
	| stream overLimit |

	super testNextPut.

	stream := self streamClass
		on: String new
		limit: self stringSize
		limitBlock: [ overLimit := true ].
	overLimit := false.
	self string
		withIndexDo: [ :each :i | 
			self
				assert: (stream nextPut: each) = each;
				assert: stream position = i;
				assert: stream contents = (self string first: i);
				deny: overLimit ].
	self string
		do: [ :each | 
			overLimit := false.
			self
				assert: (stream nextPut: each) = each;
				assert: stream position = self stringSize;
				assert: stream contents = self string;
				assert: overLimit ]