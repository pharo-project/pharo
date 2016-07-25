emitting code
readReferenceAt: byteOffset
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromHandle: (handle referenceStructAt: ';
			print: byteOffset;
			nextPutAll:' length: ';
			print: self externalTypeSize;
			nextPutAll:')' ].
