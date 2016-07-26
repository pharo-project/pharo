printing
storeOn: aStream 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' addMethod: ''';
		nextPutAll: source;
		nextPutAll: ''' toClass: '.
	class storeOn: aStream.
	aStream nextPutAll: ' inProtocols: '.
	protocols storeOn: aStream.
	aStream nextPut: $)