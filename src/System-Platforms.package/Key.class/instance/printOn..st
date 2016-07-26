printing
printOn: aStream
	aStream
		nextPutAll: 'Key ';
		nextPutAll: name;
		nextPutAll: ' code: (';
		nextPutAll: value asString;
		nextPutAll: ')'.