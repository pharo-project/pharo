printing
printOn: aStream
	aStream
		nextPutAll: 'HEPackage ( ';
		nextPutAll: packageName asString;
		nextPutAll: ' )'