printing
printOn: aStream
	super printOn: aStream.
	aStream 
		nextPutAll: ' (presentation = ';
		nextPutAll: self presentation printString;
		nextPutAll: ', property = #';
		nextPutAll: self property;
		nextPutAll: ', oldValue = ';
		nextPutAll: self oldValue gtDisplayString;
		nextPutAll: ', value = ';
		nextPutAll: self value gtDisplayString;
		nextPutAll: ')'