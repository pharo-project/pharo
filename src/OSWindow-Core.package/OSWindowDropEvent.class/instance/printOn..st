printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: ' filePath: ''';
		nextPutAll: filePath;
		nextPut: $'