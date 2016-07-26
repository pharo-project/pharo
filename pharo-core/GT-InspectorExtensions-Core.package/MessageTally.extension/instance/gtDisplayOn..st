*GT-InspectorExtensions-Core
gtDisplayOn: aStream
	self gtDisplayIdentifierOn: aStream.
	aStream 
		nextPutAll: ' (';
		nextPutAll: self tally printString;
		nextPutAll: ')'