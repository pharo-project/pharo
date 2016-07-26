printing
printOn: stream
	super printOn: stream.
	stream nextPut: $(.
	self hasEntity ifTrue: [ 
		self entity printContentTypeAndLengthOn: stream ].
	stream nextPut: $)
	