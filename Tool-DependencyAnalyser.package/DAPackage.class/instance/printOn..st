printing
printOn: aStream
	aStream
		nextPutAll: 'Dependencies on: ';
		nextPutAll: self packageName