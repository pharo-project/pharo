printing
printOn: aStream

	aStream 
		nextPutAll: self class name;
		nextPutAll: ' with ';
		print: self size;
	 	nextPutAll: ' items' .