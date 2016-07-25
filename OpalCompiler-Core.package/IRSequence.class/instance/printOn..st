printing
printOn: stream

	stream nextPutAll: 'an '.
	self class printOn: stream.
	stream space.
	stream nextPut: $(.
	self orderNumber printOn: stream.
	stream nextPut: $).
