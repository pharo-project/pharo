emitting code
offsetReadArrayAt: offsetVariableName
	self flag: #todo. "This is not working in all cases for 32/64 bits (
	externalTypeSize could be different)"
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			nextPutAll: self arrayClassName;
			nextPutAll:' fromHandle: (handle copyFrom: ';
			nextPutAll: offsetVariableName;
			nextPutAll: ' to: ';
			nextPutAll: (offsetVariableName, ' + ', (self externalTypeSize - 1) asString);
			nextPutAll:')' ].
