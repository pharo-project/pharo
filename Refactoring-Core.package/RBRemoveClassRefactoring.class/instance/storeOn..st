printing
storeOn: aStream 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' classNames: '.
	classNames asArray storeOn: aStream.
	aStream nextPut: $)