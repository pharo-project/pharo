printing
printOn: stream
	super printOn: stream.

	value isInteger 		" temp: due to a uffi bug (?) sometimes value == self"
		ifFalse: [ ^ self ].

	stream nextPut: $(;
		nextPutAll: self symbol printString;
		nextPutAll: ' [';
		nextPutAll: self value asString;
		nextPut: $];
		nextPut: $)
