printing
printOn: aStream
	aStream 
		nextPutAll: self class name;
		nextPutAll: ' with ';
		print: items size;
	 	nextPutAll: ' items'.