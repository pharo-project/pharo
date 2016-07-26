printing
printOn: aStream
	super printOn: aStream.
	aStream 
		nextPutAll: '(port='.
	self port printOn: aStream.
	aStream nextPutAll: ' oldValue='; nextPutAll: self oldValue printString; nextPut: $)
		