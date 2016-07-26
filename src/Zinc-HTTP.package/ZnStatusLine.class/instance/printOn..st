printing
printOn: stream
	super printOn: stream.
	self code ifNil: [ ^ self ].
	stream nextPut: $(.
	self printCodeAndReasonOn: stream.
	stream nextPut: $)