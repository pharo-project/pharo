printing
storeOn: aStream 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' addParameterToMethod: #';
		nextPutAll: oldSelector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' newSelector: #';
		nextPutAll: newSelector;
		nextPutAll: ' initializer: ''';
		nextPutAll: initializer;
		nextPutAll: ''')'