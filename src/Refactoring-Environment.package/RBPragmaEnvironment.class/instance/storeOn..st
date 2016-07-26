printing
storeOn: aStream
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream nextPutAll: ' keywords: '.
	keywords asArray storeOn: aStream.
	aStream nextPut: $)