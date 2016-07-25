tests
testNextPutAllEmpty
	self assert: stream position equals: 0.
	self assert: stream peekLast equals: nil.
	stream nextPutAll: ''.
	self assert: stream position equals: 0.
	self assert: stream peekLast equals: nil.