printing
printOn: aStream

	aStream
		nextPutAll: self asDateAndTime printString;
		nextPutAll: ' (';
		nextPutAll: self value asString;
		nextPut: $).