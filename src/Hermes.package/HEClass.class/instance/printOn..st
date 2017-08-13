printing
printOn: aStream
	aStream
		nextPutAll: 'HEClass ( ';
		nextPutAll: className asString;
		nextPutAll: ' )'