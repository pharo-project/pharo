tests
testNextPut
	| stream |
	stream := self newStream.
	self string
		withIndexDo: [ :each :i | 
			self
				assert: (stream nextPut: each) = each;
				assert: stream position = i;
				assert: stream contents = (self string first: i) ]