printing
objectPrintOn: aStream
	aStream
		nextPutAll: 'number of entries: ';
		nextPutAll: self size asString