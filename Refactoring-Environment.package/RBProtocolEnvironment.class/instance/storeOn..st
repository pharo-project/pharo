printing
storeOn: aStream 
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream
		nextPutAll: ' class: ';
		nextPutAll: class name;
		nextPutAll: ' protocols: '.
	protocols asArray storeOn: aStream.
	aStream nextPut: $)