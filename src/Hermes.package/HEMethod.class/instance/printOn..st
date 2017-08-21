printing
printOn: aStream
	aStream
		nextPutAll: 'HEMethod ( ';
		nextPutAll: name printString;
		nextPutAll: ' )'