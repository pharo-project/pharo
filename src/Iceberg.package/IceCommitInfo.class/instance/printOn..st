printing
printOn: aStream
	aStream nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: (self commitId first: 7);
		nextPutAll: ', ';
		print: self datetime;
		nextPutAll: ', ';
		nextPutAll: comment;
		nextPut: $)
		 