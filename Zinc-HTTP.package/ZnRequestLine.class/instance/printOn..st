printing
printOn: stream
	super printOn: stream.
	(self method notNil and: [ self uri notNil ])
		ifFalse: [ ^ self ].
	stream nextPut: $(.
	self printMethodAndUriOn: stream.
	stream nextPut: $)