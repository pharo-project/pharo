tests
testReadOnly
	handle close.
	handle := reference openWritable: false.
	self 
		should: 
			[ handle 
				at: 1
				write: #(1 2 3 )
				startingAt: 1
				count: 3 ]
		raise: Error