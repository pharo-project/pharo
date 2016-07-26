printing
storeOn: aStream
	
	aStream nextPut: $(. 
	super storeOn: aStream.
	aStream nextPutAll: ' baseClass:'.
	self theNonMetaClass storeOn: aStream.
	aStream nextPut: $).
	
