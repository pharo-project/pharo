printing
printOn: aStream
	super printOn: aStream.
	aStream 
		nextPutAll: ' (presentation = ';
		nextPutAll: self presentation printString;
		nextPutAll: ', property = #';
		nextPutAll: self property;
		nextPutAll: ', oldValue = ';
		nextPutAll: self oldValue asString;
		nextPutAll: ', value = ';
		nextPutAll: self value asString;
		nextPutAll: ')'