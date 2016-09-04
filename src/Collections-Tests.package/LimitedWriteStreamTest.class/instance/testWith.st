tests
testWith
	"like on: except it starts writing at the end of its argument collection,
	and the initial position is the collection size and the initial contents
	is the collection"
	| stream overLimit |

	super testWith.

	stream := self streamClass with: self string.
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