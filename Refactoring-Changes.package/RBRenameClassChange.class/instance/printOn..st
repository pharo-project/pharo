printing
printOn: aStream 
	aStream
		nextPutAll: self oldName;
		nextPutAll: ' rename: ';
		print: self newName;
		nextPut: $!