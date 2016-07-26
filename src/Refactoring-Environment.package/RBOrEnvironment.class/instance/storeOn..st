printing
storeOn: aStream 
	aStream nextPut: $(.
	environment storeOn: aStream.
	aStream nextPutAll: ' | '.
	orEnvironment storeOn: aStream.
	aStream nextPut: $)