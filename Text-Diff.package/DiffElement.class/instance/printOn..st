printing
printOn: aStream

	super printOn: aStream.
	aStream
		nextPut: $(;
		print: hash;
		nextPutAll: ', ';
		print: string;
		nextPutAll: ', ';
		print: (match class == self class);
		nextPut: $)