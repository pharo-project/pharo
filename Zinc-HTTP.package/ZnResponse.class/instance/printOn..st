printing
printOn: stream
	super printOn: stream.
	stream nextPut: $(.
	self statusLine printCodeAndReasonOn: stream.
	self hasEntity ifTrue: [ 
		stream space.
		self entity printContentTypeAndLengthOn: stream ].
	stream nextPut: $)
	