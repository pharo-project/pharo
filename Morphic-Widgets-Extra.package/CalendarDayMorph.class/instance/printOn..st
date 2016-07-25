printing
printOn: aStream

	aStream
		print: self class;
		nextPutAll: ' (';
		print: self date;
		nextPut: $)