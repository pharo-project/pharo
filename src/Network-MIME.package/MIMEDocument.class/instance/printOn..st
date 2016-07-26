printing
printOn: aStream
	aStream nextPutAll: self class name;
		nextPutAll: ' (';
		nextPutAll: self mimeType asString;
		nextPutAll: ', '.
	contents
		ifNotNil: [aStream
			nextPutAll: self contents size printString;
			nextPutAll: ' bytes)']
		ifNil: [aStream nextPutAll: 'unknown size)'].