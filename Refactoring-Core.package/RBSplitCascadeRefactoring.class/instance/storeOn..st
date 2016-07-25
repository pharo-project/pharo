printing
storeOn: aStream 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' split: '.
	selectedInterval storeOn: aStream.
	aStream
		nextPutAll: ' inMethod: #';
		nextPutAll: selector;
		nextPutAll: ' forClass: '.
	class storeOn: aStream.
	aStream nextPut: $)