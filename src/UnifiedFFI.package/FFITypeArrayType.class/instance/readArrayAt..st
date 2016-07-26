emitting code
readArrayAt: byteOffset
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^'. 
		stream nextPutAll: self arrayClassName.
		stream
			nextPutAll:' fromHandle: (handle copyFrom: ';
			print: byteOffset;
			nextPutAll: ' to: ';
			print: byteOffset + self externalTypeSize - 1;
			nextPutAll:')' ].
