printing
storeOn: aStream 
	aStream nextPut: $(; nextPutAll: self class name; nextPutAll: ' onEnvironment: '.
	environment storeOn: aStream.
	aStream nextPutAll: ' packageNames: '.
	self packageNames asArray storeOn: aStream.
	aStream nextPut: $)