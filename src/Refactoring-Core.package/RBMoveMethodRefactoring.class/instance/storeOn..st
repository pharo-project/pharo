printing
storeOn: aStream 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: selector;
		nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' variable: ''';
		nextPutAll: variable;
		nextPutAll: ''')'