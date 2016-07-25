emitting code
readHandleAt: byteOffset
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromInteger: (handle ', self externalType atomicSelector,' ';
			print: byteOffset;
			nextPutAll:')' ].
