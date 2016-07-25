printing
storeOn: aStream
	aStream
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' named: '.
	name storeOn: aStream.
	aStream nextPut: $)